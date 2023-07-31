{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Scanner where -- (CodeError (..), ScannerError (..), printErrs, scanFile, ppPrintErr)

import Control.Monad.ST
import Control.Monad.Writer.Strict
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Function ((&))
import Data.Ratio ((%))
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace (traceShow, traceShowId)
import FlatParse.Common.Parser (STMode)
import FlatParse.Stateful
import Token

data ScannerError
  = UnexpectedCharacter Pos Char
  | UnterminatedString Pos
  | InvalidNumberLiteral Pos ByteString
  | FailedNumber
  deriving (Show)

data CodeError
  = CUnexpectedCharacter Char (Int, Int)
  | CUnterminatedString (Int, Int)
  deriving (Show)

newtype ScanErr = ScanErr (Vector ScannerError)
  deriving (Show)

printErrs :: MonadIO m => ByteString -> Vector ScannerError -> m ()
printErrs completeBS vec =
  if V.null vec
    then liftIO $ B.putStrLn "No Errors!"
    else do
      mapM_ ppPrintErr finalLi
  where
    (charVec, posVec) =
      let f' (UnexpectedCharacter pos ch) = (CUnexpectedCharacter ch, pos)
          f' (UnterminatedString pos) = (CUnterminatedString, pos)
       in V.unzip $ fmap f' vec

    linecols :: [(Int, Int)]
    linecols = posLineCols completeBS $ V.toList posVec

    finalLi :: [CodeError]
    finalLi =
      let charList = V.toList charVec
          f cErrorConstructor (l, c) = cErrorConstructor (l + 1, c + 1) -- start col/line numbers from 1
       in zipWith f charList linecols

ppPrintErr :: MonadIO m => CodeError -> m ()
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

scanFile
  :: ByteString
  -> ST s (Either CodeError (Vector ScannerError, Vector Token, ByteString))
scanFile bs = do
  stRef <- newSTRef $ ScanErr V.empty

  res <- runParserST scanTokens stRef 0 bs

  (ScanErr errVec) <- readSTRef stRef
  pure $ case res of
    OK vec _ restOfBs -> Right (errVec, vec, restOfBs)
    Err e -> case e of
      UnexpectedCharacter pos ch ->
        let [linecol] = posLineCols bs [pos] -- only one position, the pattern will always be matched
         in Left $ CUnexpectedCharacter ch linecol
      UnterminatedString pos ->
        let [linecol] = posLineCols bs [pos]
         in Left $ CUnterminatedString linecol
    _ -> error "failure should never propagate here"

simpleScanToken :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
simpleScanToken =
  $( switch
      [|
        case _ of
          "(" -> pure LEFT_PAREN
          ")" -> pure RIGHT_PAREN
          "{" -> pure LEFT_BRACE
          "}" -> pure RIGHT_BRACE
          "," -> pure COMMA
          "." -> pure DOT
          "-" -> pure MINUS
          "+" -> pure PLUS
          ";" -> pure SEMICOLON
          "*" -> pure STAR
          "!" -> pure BANG
          "!=" -> pure BANG_EQUAL
          "=" -> pure EQUAL
          "==" -> pure EQUAL_EQUAL
          "<" -> pure LESS
          "<=" -> pure LESS_EQUAL
          ">" -> pure GREATER
          ">=" -> pure GREATER_EQUAL
          "/" -> pure SLASH
          "\"" -> parseString
          _ ->
            withError
              parseNumber
              (\FailedNumber -> skipMany (skipSatisfyAscii isDigit) >> scanToken)
        |]
   )

-- {-# INLINE simpleScanToken #-}

appendSTRefSCanErrors :: STRef s ScanErr -> ScannerError -> ST s ()
appendSTRefSCanErrors stref er = modifySTRef' stref (apndScanningErrors er)
  where
    apndScanningErrors codeError (ScanErr v) = ScanErr $ V.snoc v codeError

reportError :: ScannerError -> ParserT (STMode s) (STRef s ScanErr) e ()
reportError e = do
  stref <- ask
  liftST $ appendSTRefSCanErrors stref e

scanToken :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
scanToken = do
  skipWhiteSpace
  simpleScanToken <|> do
    pos <- getPos
    ch <- lookahead anyChar
    reportError $ UnexpectedCharacter pos ch
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
parseString = STRING <$> parse
  where
    parse = do
      pos <- lookahead (skipBack 1 >> getPos)
      bs <- byteStringOf $ skipMany (skipSatisfyAscii (/= '"'))
      branch
        advance
        (pure bs)
        ( do
            reportError $ UnterminatedString pos
            err $ UnterminatedString pos
        )

parseNumber :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
parseNumber =
  parse
    <|> (skipMany (skipSatisfyAscii (\n -> isDigit n || n == '.')) >> err undefined)
  where
    parse = do
      initialPos <- getPos
      initChar <- traceShowId <$> lookahead anyChar

      firstBs <- byteStringOf $ skipMany (skipSatisfyAscii isDigit)

      nBeforeDot <- case B.readInt firstBs of
        Just (numBeforeDot, _) -> pure numBeforeDot
        Nothing -> do
          reportError $ InvalidNumberLiteral initialPos firstBs
          failed

      let n1 = fromIntegral nBeforeDot
      branch
        (skipSatisfyAscii (== '.'))
        ( do
            chPos <- getPos
            ch <- lookahead anyChar
            if isDigit ch
              then do
                secondBs <- byteStringOf $ skipMany (skipSatisfyAscii isDigit)
                let (Just (numAfterDot, _)) = B.readInt secondBs
                    n2 = fromIntegral numAfterDot / (10 ^ B.length secondBs)
                pure $ NUMBER (n1 + n2)
              else do
                reportError $ InvalidNumberLiteral initialPos (B.snoc firstBs '.')
                failed
                -- err
                --  $ UnexpectedCharacter chPos ch
        )
        ( do
            fails eof
            pure $ traceShowId $ NUMBER n1
        )
