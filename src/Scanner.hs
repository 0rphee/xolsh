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
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace (trace, traceShow, traceShowId)
import FlatParse.Common.Parser (STMode)
import FlatParse.Stateful
import Token

data ScannerError
  = UnexpectedCharacter Pos Char
  | UnterminatedString Pos
  | InvalidNumberLiteral Pos Pos ByteString
  deriving (Show)

data CodeError
  = CUnexpectedCharacter Char (Int, Int)
  | CUnterminatedString (Int, Int)
  | CInvalidNumberLiteral (Int, Int) ByteString
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
          f' (InvalidNumberLiteral pos _ bs) = ((`CInvalidNumberLiteral` bs), pos)
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
        CInvalidNumberLiteral (l, c) bs -> (l, c, "InvalidNumberLiteral '" <> bs <> "'")
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
      InvalidNumberLiteral pos _ errBs ->
        let [linecol] = posLineCols bs [pos]
         in Left $ CInvalidNumberLiteral linecol errBs
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
  simpleScanToken <|> parseNumber <|> parseKeywAndIdentif <|> do
    pos <- getPos
    ch <- lookahead anyChar
    reportError $ UnexpectedCharacter pos ch
    failed

scanTokens :: ParserT (STMode s) (STRef s ScanErr) ScannerError (Vector Token)
scanTokens = tryUntilEOF scanToken

withAnyResult
  :: ParserT st r e a -- initial parser
  -> (a -> ParserT st r e res) -- success
  -> ParserT st r e res -- failure
  -> (e -> ParserT st r e res) -- error
  -> ParserT st r e res
withAnyResult (ParserT initial) whenSuccess (ParserT whenFailure) whenError =
  ParserT \fp !r eob s n st ->
    case initial fp r eob s n st of
      OK# st' a s' n' -> runParserT# (whenSuccess a) fp r eob s' n' st'
      Fail# st' -> whenFailure fp r eob s n st'
      Err# st' e -> runParserT# (whenError e) fp r eob s n st'
{-# INLINE withAnyResult #-}

tryUntilEOF
  :: ParserST s (STRef s ScanErr) ScannerError a
  -> ParserST s (STRef s ScanErr) ScannerError (Vector a)
tryUntilEOF p = go
  where
    go =
      withAnyResult
        p
        (\a -> V.cons a <$> go)
        (branch (skip 1) go (pure V.empty))
        ( \e -> do
            reportError e
            case e of
              UnterminatedString _ -> setPos endPos >> go
              InvalidNumberLiteral _ failingPos _ -> setPos failingPos >> go
              _ -> do
                branch (skip 1) go (pure V.empty)
        )
{-# INLINE tryUntilEOF #-}

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
        (err $ UnterminatedString pos)

parseNumber :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
parseNumber =
  parse
  where
    parse = do
      initialPos <- getPos

      firstBs <- byteStringOf $ skipMany (skipSatisfyAscii isDigit)

      nBeforeDot <- case B.readInt firstBs of
        Just (numBeforeDot, _) -> pure numBeforeDot
        Nothing -> do
          failed

      let n1 = fromIntegral nBeforeDot
      branch
        (skipSatisfyAscii (== '.'))
        ( do
            dotPos <- getPos
            cmaybe <- optional (lookahead anyChar)
            case cmaybe of
              Nothing -> err $ InvalidNumberLiteral initialPos dotPos (B.snoc firstBs '.')
              Just ch ->
                if isDigit ch
                  then do
                    secondBs <- byteStringOf $ skipMany (skipSatisfyAscii isDigit)
                    let (Just (numAfterDot, _)) = B.readInt secondBs
                        n2 = fromIntegral numAfterDot / (10 ^ B.length secondBs)
                    pure $ NUMBER (n1 + n2)
                  else do
                    skipBack 1
                    invalidRest <-
                      byteStringOf $
                        skipMany
                          (skipSatisfyAscii (\c -> c /= ' ' || c /= '\n' || c /= '\t' || c /= '\n'))
                    finalPos <- getPos
                    err $ InvalidNumberLiteral initialPos finalPos (firstBs <> invalidRest)
        )
        (pure $ NUMBER n1)

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isLatinLetter c || isDigit c

parseKeywAndIdentif :: ParserT st r e Token
parseKeywAndIdentif =
  let checkNext r =
        branch
          (lookahead (skipSatisfyAscii isAlphaNumeric))
          (skipMany (skipSatisfyAscii isAlphaNumeric) >> pure IDENTIFIER)
          (pure r)
   in $( switch
          [|
            case _ of
              "and" -> checkNext AND
              "class" -> checkNext CLASS
              "else" -> checkNext ELSE
              "false" -> checkNext FALSE
              "for" -> checkNext FOR
              "fun" -> checkNext FUNN
              "if" -> checkNext IF
              "nil" -> checkNext NIL
              "or" -> checkNext OR
              "print" -> checkNext PRINT
              "return" -> checkNext RETURN
              "super" -> checkNext SUPER
              "this" -> checkNext THIS
              "true" -> checkNext TRUE
              "var" -> checkNext VAR
              "while" -> checkNext WHILE
              _ -> do
                skipSatisfyAscii isLatinLetter
                skipMany (skipSatisfyAscii isAlphaNumeric)
                pure IDENTIFIER
            |]
       )
