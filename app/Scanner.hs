{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Scanner where

import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeIOToST)
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
import Unsafe.Coerce (unsafeCoerce)

data ScannerError = UnexpectedCharacter Char Pos deriving (Show)

data CodeError = ERR Char (Int, Int) deriving (Show)

newtype ScanErr = ScanErr (Vector ScannerError)
  deriving (Show)

printErrs :: (MonadIO m) => ByteString -> Vector ScannerError -> m ()
printErrs completeBS vec =
  if V.null vec
    then liftIO $ B.putStrLn "No Errors!"
    else do
      mapM_ ppPrintErrs finalLi
  where
    (charVec, posVec) =
      let f' (UnexpectedCharacter ch pos) = (ch, pos)
       in V.unzip $ fmap f' vec
    linecols :: [(Int, Int)]
    linecols = posLineCols completeBS $ V.toList posVec

    finalLi :: [(Char, Int, Int)]
    finalLi =
      let charList = V.toList charVec
          f char' (line, col) = (char', line, col)
       in zipWith f charList linecols

    ppPrintErrs (ch, line, col) = liftIO $ B.putStrLn str
      where
        str =
          "[line: "
            <> (line & show & B.pack)
            <> ", col: "
            <> (col & show & B.pack)
            <> "] Error: UnexpectedCharacter '"
            <> B.singleton ch
            <> "'"

scanFile :: ByteString -> ST s (Either CodeError (Vector ScannerError, Vector Token, ByteString))
scanFile bs = do
  stRef <- newSTRef $ ScanErr V.empty

  res <- runParserST scanTokens stRef 0 bs

  (ScanErr errVec) <- readSTRef stRef
  pure $ case res of
    OK vec _ restOfBs -> Right (errVec, vec, restOfBs)
    Err (UnexpectedCharacter ch pos) ->
      let [linecol] = posLineCols bs [pos] -- only one position, the pattern will always be matched
       in Left $ ERR ch linecol
    _ -> error "failure should never propagate here"

simpleScanToken :: ParserT st r e Token
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
            |]
       )

appendSTRefSCanErrors :: STRef s ScanErr -> ScannerError -> ST s ()
appendSTRefSCanErrors stref er = modifySTRef' stref (apndScanningErrors er)
  where
    apndScanningErrors codeError (ScanErr v) = ScanErr $ V.snoc v codeError

scanToken :: ParserT (STMode s) (STRef s ScanErr) e Token
scanToken = do
  skipWhiteSpace
  simpleScanToken <|> do
    pos <- getPos
    ch <- lookahead anyChar
    stref :: STRef s ScanErr <- ask
    liftST $ appendSTRefSCanErrors stref $ UnexpectedCharacter ch pos
    failed

scanTokens :: ParserT (STMode s) (STRef s ScanErr) e (Vector Token)
scanTokens = mymany' scanToken

mymany' :: ParserT st r e Token -> ParserT st r e (Vector Token)
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
        Err# st e -> Err# st e

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
