{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Scanner
  ( CodeError (..)
  , ScannerError (..)
  , printErrs
  , scanFile
  , ppPrintErr
  , skipWhiteSpace
  , parseKeywAndIdentif
  , simpleScanParser
  , simpleScanParserS
  , tryUntilEOF
  , parseNumber
  , simpleScanToken
  )
where

import Control.Monad.ST
import Control.Monad.Writer.Strict
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Function ((&))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import FlatParse.Common.Parser (STMode)
import FlatParse.Stateful
import Token

data ScannerError
  = UnexpectedCharacter !Pos !Char
  | UnterminatedString !Pos
  | InvalidNumberLiteral !Pos !Pos !ByteString
  deriving (Show)

data CodeError
  = CUnexpectedCharacter !Char !(Int, Int)
  | CUnterminatedString !(Int, Int)
  | CInvalidNumberLiteral !(Int, Int) !ByteString
  deriving (Show)

newtype ScanErr = ScanErr (Vector ScannerError)
  deriving (Show)

type ToToken = (Pos, TokPos -> Token)

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

simpleScanParser
  :: ByteString
  -> ParserT (STMode s) (STRef s ScanErr) e (Vector ToToken)
  -> ST s (Result e (Vector Token))
simpleScanParser bs p = do
  stref <- newSTRef $ ScanErr V.empty
  res <- runParserST p stref 0 bs
  (ScanErr _errVec) <- readSTRef stref
  pure $ annotateTokenVector bs <$> res

simpleScanParserS
  :: ByteString
  -> ParserT (STMode s) (STRef s ScanErr) e ToToken
  -> ST s (Result e Token)
simpleScanParserS bs p = do
  stref <- newSTRef $ ScanErr V.empty
  res <- runParserST p stref 0 bs
  (ScanErr _errVec) <- readSTRef stref
  pure $ V.head . annotateTokenVector bs . V.singleton <$> res

annotateTokenVector :: ByteString -> Vector ToToken -> Vector Token
annotateTokenVector bs vec =
  let (linecolsV, toTokVec) = V.unzip vec
      lineColsL =
        posLineCols bs $
          V.toList linecolsV
   in V.zipWith id toTokVec (V.fromList lineColsL)

scanFile
  :: ByteString
  -> ST s (Either CodeError (Vector ScannerError, Vector Token, ByteString))
scanFile bs = do
  stRef <- newSTRef $ ScanErr V.empty

  res <- runParserST scanTokens stRef 0 bs

  (ScanErr errVec) <- readSTRef stRef
  pure $ case res of
    OK vec _ restOfBs ->
      Right (errVec, annotateTokenVector bs vec, restOfBs)
    Err e -> case e of
      -- only one position, the pattern will always be matched
      UnexpectedCharacter pos ch ->
        Left $ CUnexpectedCharacter ch (errorGetLineCol pos)
      UnterminatedString pos ->
        Left $ CUnterminatedString (errorGetLineCol pos)
      InvalidNumberLiteral pos _ errBs ->
        Left $ CInvalidNumberLiteral (errorGetLineCol pos) errBs
    _ -> error "Failure should never propagate here"
  where
    errorGetLineCol :: Pos -> (Int, Int)
    errorGetLineCol pos =
      fromMaybe
        ( error "Reached the impossible, position not found in original input bytestring"
        )
        . listToMaybe
        $ posLineCols bs [pos]

retToTok :: (TokPos -> Token) -> ParserT st r e ToToken
retToTok x = do
  posInBS <- getPos
  pure (posInBS, x)

simpleScanToken
  :: ParserT (STMode s) (STRef s ScanErr) ScannerError ToToken
simpleScanToken =
  $( switch
      [|
        case _ of
          "(" -> retToTok LEFT_PAREN
          ")" -> retToTok RIGHT_PAREN
          "{" -> retToTok LEFT_BRACE
          "}" -> retToTok RIGHT_BRACE
          "," -> retToTok COMMA
          "." -> retToTok DOT
          "-" -> retToTok MINUS
          "+" -> retToTok PLUS
          ";" -> retToTok SEMICOLON
          "*" -> retToTok STAR
          "!" -> retToTok BANG
          "!=" -> retToTok BANG_EQUAL
          "=" -> retToTok EQUAL
          "==" -> retToTok EQUAL_EQUAL
          "<" -> retToTok LESS
          "<=" -> retToTok LESS_EQUAL
          ">" -> retToTok GREATER
          ">=" -> retToTok GREATER_EQUAL
          "/" -> retToTok SLASH
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

scanToken :: ParserT (STMode s) (STRef s ScanErr) ScannerError ToToken
scanToken = do
  skipWhiteSpace
  simpleScanToken <|> parseNumber <|> parseKeywAndIdentif <|> do
    pos <- getPos
    ch <- lookahead anyChar
    reportError $ UnexpectedCharacter pos ch
    failed

scanTokens :: ParserST s (STRef s ScanErr) ScannerError (Vector ToToken)
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

parseString
  :: ParserT (STMode s) (STRef s ScanErr) ScannerError ToToken
parseString = parse
  where
    parse = do
      pos <- lookahead (skipBack 1 >> getPos)
      bs <- byteStringOf $ skipMany (skipSatisfyAscii (/= '"'))
      branch
        advance
        (retToTok $ STRING bs)
        (err $ UnterminatedString pos)

parseNumber
  :: ParserT (STMode s) (STRef s ScanErr) ScannerError ToToken
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
                    let (numAfterDot, _restOfBs) =
                          fromMaybe
                            (error "Reached the impossible: number parsing should be guaranteed")
                            (B.readInt secondBs)
                        n2 = fromIntegral numAfterDot / (10 ^ B.length secondBs)
                    retToTok $ NUMBER (n1 + n2)
                  else do
                    skipBack 1
                    invalidRest <-
                      byteStringOf $
                        skipMany
                          (skipSatisfyAscii (\c -> c /= ' ' || c /= '\n' || c /= '\t' || c /= '\n'))
                    finalPos <- getPos
                    err $ InvalidNumberLiteral initialPos finalPos (firstBs <> invalidRest)
        )
        (retToTok $ NUMBER n1)

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isLatinLetter c || isDigit c

parseKeywAndIdentif :: ParserT st r e ToToken
parseKeywAndIdentif =
  let checkNext r =
        branch
          (lookahead (skipSatisfyAscii isAlphaNumeric))
          (skipMany (skipSatisfyAscii isAlphaNumeric) >> retToTok IDENTIFIER)
          (retToTok r)
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
                retToTok IDENTIFIER
            |]
       )
