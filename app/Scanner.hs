{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Scanner (CodeError (..), ScannerError (..), printErrs, scanFile, ppPrintErr) where

import Control.Monad.ST
import Control.Monad.Writer.Strict
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Function ((&))
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import FlatParse.Common.Parser (STMode)
import FlatParse.Stateful
import Token

data ScannerError
  = UnexpectedCharacter Char Pos
  | UnterminatedString Pos
  deriving (Show)

data CodeError
  = CUnexpectedCharacter Char (Int, Int)
  | CUnterminatedString (Int, Int)
  deriving (Show)

newtype ScanErr = ScanErr (Vector ScannerError)
  deriving (Show)

printErrs :: (MonadIO m) => ByteString -> Vector ScannerError -> m ()
printErrs completeBS vec =
  if V.null vec
    then liftIO $ B.putStrLn "No Errors!"
    else do
      mapM_ ppPrintErr finalLi
  where
    (charVec, posVec) =
      let f' (UnexpectedCharacter ch pos) = (CUnexpectedCharacter ch, pos)
          f' (UnterminatedString pos) = (CUnterminatedString, pos)
       in V.unzip $ fmap f' vec

    linecols :: [(Int, Int)]
    linecols = posLineCols completeBS $ V.toList posVec

    finalLi :: [CodeError]
    finalLi =
      let charList = V.toList charVec
          f cErrorConstructor (l, c) = cErrorConstructor (l + 1, c + 1) -- start col/line numbers from 1
       in zipWith f charList linecols

ppPrintErr :: (MonadIO m) => CodeError -> m ()
ppPrintErr e = liftIO $ B.putStrLn (lineColStr <> restStr)
  where
    (line, col, restStr) =
      case e of
        CUnexpectedCharacter ch (l, c) -> (l, c, "UnexpectedCharacter '" <> B.singleton ch <> "'")
        CUnterminatedString (l, c) -> (l, c, "UnterminatedString")
    lineColStr =
      "[line: "
        <> (line & show & B.pack)
        <> ", col: "
        <> (col & show & B.pack)
        <> "] Error: "

scanFile :: ByteString -> ST s (Either CodeError (Vector ScannerError, Vector Token, ByteString))
scanFile bs = do
  stRef <- newSTRef $ ScanErr V.empty

  res <- runParserST scanTokens stRef 0 bs

  (ScanErr errVec) <- readSTRef stRef
  pure $ case res of
    OK vec _ restOfBs -> Right (errVec, vec, restOfBs)
    Err e -> case e of
      UnexpectedCharacter ch pos ->
        let [linecol] = posLineCols bs [pos] -- only one position, the pattern will always be matched
         in Left $ CUnexpectedCharacter ch linecol
      UnterminatedString pos ->
        let [linecol] = posLineCols bs [pos]
         in Left $ CUnterminatedString linecol
    _ -> error "failure should never propagate here"

simpleScanToken :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
simpleScanToken =
  let returnTok = pure . (`Token` Nothing)
   in $( switch
          [|
            case _ of
              "(" -> returnTok LEFT_PAREN
              ")" -> returnTok RIGHT_PAREN
              "{" -> returnTok LEFT_BRACE
              "}" -> returnTok RIGHT_BRACE
              "," -> returnTok COMMA
              "." -> returnTok DOT
              "-" -> returnTok MINUS
              "+" -> returnTok PLUS
              ";" -> returnTok SEMICOLON
              "*" -> returnTok STAR
              "!" -> returnTok BANG
              "!=" -> returnTok BANG_EQUAL
              "=" -> returnTok EQUAL
              "==" -> returnTok EQUAL_EQUAL
              "<" -> returnTok LESS
              "<=" -> returnTok LESS_EQUAL
              ">" -> returnTok GREATER
              ">=" -> returnTok GREATER_EQUAL
              "/" -> returnTok SLASH
              "\"" -> parseString
            |]
       )
{-# INLINE simpleScanToken #-}

appendSTRefSCanErrors :: STRef s ScanErr -> ScannerError -> ST s ()
appendSTRefSCanErrors stref er = modifySTRef' stref (apndScanningErrors er)
  where
    apndScanningErrors codeError (ScanErr v) = ScanErr $ V.snoc v codeError

scanToken :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
scanToken = do
  skipWhiteSpace
  simpleScanToken <|> do
    pos <- getPos
    ch <- lookahead anyChar
    stref :: STRef s ScanErr <- ask
    liftST $ appendSTRefSCanErrors stref $ UnexpectedCharacter ch pos
    failed

scanTokens :: ParserT (STMode s) (STRef s ScanErr) ScannerError (Vector Token)
scanTokens = mymany' scanToken

-- mymany' :: ParserT st r e Token -> ParserT st r e (Vector Token)
mymany' :: ParserT st r ScannerError a -> ParserT st r ScannerError (Vector a)
mymany' i@(ParserT f) = ParserT go
  where
    go fp !stref eob s n st =
      case f fp stref eob s n st of
        OK# st a s n ->
          case go fp stref eob s n st of
            OK# st as s n -> OK# st (V.cons a as) s n
            x -> x
        Fail# st ->
          let (ParserT b) =
                branch
                  eof
                  (pure V.empty)
                  ( do
                      skipWhiteSpace
                      branch advance (mymany' i) (pure V.empty)
                  )
           in b fp stref eob s n st
        Err# st e -> case e of
          UnterminatedString _ ->
            let ParserT h = setPos endPos >> mymany' i
             in h fp stref eob s n st
          _ -> Err# st e

skipLineComment :: ParserT st r e ()
skipLineComment =
  branch eof (pure ()) $
    withOption
      anyWord8
      ( \case
          10 -> skipWhiteSpace -- '\n'
          _ -> skipLineComment
      )
      (pure ())

advance :: ParserT st r e ()
advance = skip 1

skipWhiteSpace :: ParserT st r e ()
skipWhiteSpace =
  $( switch
      [|
        case _ of
          " " -> skipWhiteSpace
          "\r" -> skipWhiteSpace
          "\t" -> skipWhiteSpace
          "\n" -> skipWhiteSpace
          _ -> branch $(string "//") skipLineComment (pure ())
        |]
   )

parseString :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
parseString = Token STRING . Just <$> parse
  where
    parse = do
      pos <- lookahead (skipBack 1 >> getPos)
      bs <- byteStringOf $ skipMany (skipSatisfyAscii (/= '"'))
      branch
        advance
        (pure bs)
        ( do
            stref :: STRef s ScanErr <- ask
            liftST $ appendSTRefSCanErrors stref $ UnterminatedString pos
            err $ UnterminatedString pos
        )
