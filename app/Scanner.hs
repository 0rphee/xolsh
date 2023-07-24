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
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace (traceShow)
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
      liftIO $ print vec
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
          "UnexpectedCharacter '"
            <> B.singleton ch
            <> "' at l:"
            <> B.pack (show $ line + 1)
            <> ", c:"
            <> B.pack (show $ col + 1)

unsafeRunParserST :: ParserST s (STRef s c) e a -> STRef s c -> Int -> ByteString -> ST s (Result e a)
unsafeRunParserST pst !r i buf = unsafeIOToST (runParserIO (unsafeCoerce pst) (unsafeCoerce r) i buf)

scanFile :: ByteString -> ST s (Either CodeError (Vector ScannerError, Vector Token, ByteString))
scanFile bs = do
  stRef <- newSTRef $ ScanErr V.empty

  res <- unsafeRunParserST scanTokens stRef 0 bs

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

withError :: ParserT st r e a -> (e -> ParserT st r e a) -> ParserT st r e a
withError (ParserT f) hdl =
  ParserT $ \foreignPtrContents r eob s int st -> case f foreignPtrContents r eob s int st of
    Err# st' er -> case hdl er of
      ParserT g -> g foreignPtrContents r eob s int st'
    x -> x

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

-- mymany' :: ParserT (STMode s) (STRef s ScanErr) e Token -> ParserT (STMode s) (STRef s ScanErr) e (Vector Token)
mymany' :: ParserT st r e Token -> ParserT st r e (Vector Token)
mymany' i@(ParserT f) = ParserT go
  where
    -- go :: ForeignPtrContents -> STRef s ScanErr -> Addr# -> Addr# -> Int# -> STMode s -> Res# (STMode s) e a
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
                      rest <- traceRest
                      traceShow rest $ branch advance (mymany' i) (pure V.empty)
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
