{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Scanner
  ( CodeError (..)
  , ScannerError (..)
  , printErrs
  , scanFile
  , ppPrintErr
  , skipWhiteSpace
  , parseKeywAndIdentif
  , simpleScanParser
  , simpleScanParserSingle
  , tryUntilEOF
  , parseNumber
  , simpleScanToken
  , while
  , godScan
  , simpleScanParserU
  , testGod
  )
where

import Control.Monad.ST
import Control.Monad.Writer.Strict
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char (isLetter)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import FlatParse.Common.Parser (STMode)
import FlatParse.Stateful
import Token
import VectorBuilder.Alternative qualified as VBA

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

simpleScanParserU
  :: ByteString
  -> ParserT (STMode s) (STRef s ScanErr) e v
  -> ST s (Result e v)
simpleScanParserU bs p = do
  stref <- newSTRef $ ScanErr V.empty
  res <- runParserST p stref 0 bs
  (ScanErr _errVec) <- readSTRef stref
  pure res

simpleScanParser
  :: ByteString
  -> ParserT (STMode s) (STRef s ScanErr) e (Vector Token)
  -> ST s (Result e (Vector Token))
simpleScanParser bs p = do
  stref <- newSTRef $ ScanErr V.empty
  res <- runParserST p stref 0 bs
  (ScanErr _errVec) <- readSTRef stref
  pure res

simpleScanParserSingle
  :: ByteString
  -> ParserT (STMode s) (STRef s ScanErr) e Token
  -> ST s (Result e Token)
simpleScanParserSingle bs p = do
  stref <- newSTRef $ ScanErr V.empty
  res <- runParserST p stref 0 bs
  (ScanErr _errVec) <- readSTRef stref
  pure res

scanFile
  :: ByteString
  -> ST s (Either CodeError (Vector ScannerError, Vector Token, ByteString))
scanFile bs = do
  stRef <- newSTRef $ ScanErr V.empty

  res <- runParserST scanTokens stRef 0 bs

  (ScanErr errVec) <- readSTRef stRef
  pure $ case res of
    OK vec _ restOfBs ->
      Right (errVec, vec, restOfBs)
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

simpleScanToken
  :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
simpleScanToken = do
  let toTok v = Token v <$> getPos
   in $( switch
          [|
            case _ of
              "(" -> toTok LEFT_PAREN
              ")" -> toTok RIGHT_PAREN
              "{" -> toTok LEFT_BRACE
              "}" -> toTok RIGHT_BRACE
              "," -> toTok COMMA
              "." -> toTok DOT
              "-" -> toTok MINUS
              "+" -> toTok PLUS
              ";" -> toTok SEMICOLON
              "*" -> toTok STAR
              "!" -> toTok BANG
              "!=" -> toTok BANG_EQUAL
              "=" -> toTok EQUAL
              "==" -> toTok EQUAL_EQUAL
              "<" -> toTok LESS
              "<=" -> toTok LESS_EQUAL
              ">" -> toTok GREATER
              ">=" -> toTok GREATER_EQUAL
              "/" -> toTok SLASH
              "\"" -> parseString
            |]
       )

-- {-# INLINE simpleScanToken #-}

reportError :: ScannerError -> ParserT (STMode s) (STRef s ScanErr) e ()
reportError e = do
  stref <- ask
  liftST $ appendSTRefSCanErrors stref e
  where
    appendSTRefSCanErrors :: STRef s ScanErr -> ScannerError -> ST s ()
    appendSTRefSCanErrors stref er = modifySTRef' stref (apndScanningErrors er)
      where
        apndScanningErrors codeError (ScanErr v) = ScanErr $ V.snoc v codeError

scanToken :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
scanToken = do
  skipWhiteSpace
  simpleScanToken <|> parseNumber <|> parseKeywAndIdentif <|> do
    pos <- getPos
    ch <- lookahead anyChar
    reportError $ UnexpectedCharacter pos ch
    failed

scanTokens :: ParserST s (STRef s ScanErr) ScannerError (Vector Token)
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

while
  :: Monoid b
  => ParserT st r e a
  -- ^  While this parser succceds
  -> ParserT st r e b
  -- ^ Run this parser
  -> ParserT st r e b
while condition continue = branch condition (while condition continue) (pure mempty)

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
  where
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

parseString
  :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
parseString = parse
  where
    parse = do
      pos <- lookahead (skipBack 1 >> getPos)
      bs <- byteStringOf $ skipMany (skipSatisfy (/= '"'))
      branch
        (skip 1)
        (pure $ Token (STRING bs) pos)
        (err $ UnterminatedString pos)

parseNumber
  :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
parseNumber =
  parse
  where
    parse = do
      initialPos <- getPos

      firstBs <- byteStringOf $ skipMany (skipSatisfy isDigit)

      nBeforeDot <- case B.readInt firstBs of
        Just (numBeforeDot, _) -> pure numBeforeDot
        Nothing -> do
          failed

      let n1 = fromIntegral nBeforeDot
      branch
        (skipSatisfy (== '.'))
        ( do
            dotPos <- getPos
            cmaybe <- optional (lookahead anyChar)
            case cmaybe of
              Nothing -> err $ InvalidNumberLiteral initialPos dotPos (B.snoc firstBs '.')
              Just ch ->
                if isDigit ch
                  then do
                    secondBs <- byteStringOf $ skipMany (skipSatisfy isDigit)
                    let (numAfterDot, _restOfBs) =
                          fromMaybe
                            (error "Reached the impossible: number parsing should be guaranteed")
                            (B.readInt secondBs)
                        n2 = fromIntegral numAfterDot / (10 ^ B.length secondBs)
                    pure $ Token (NUMBER (n1 + n2)) initialPos
                  else do
                    skipBack 1
                    invalidRest <-
                      byteStringOf $
                        skipMany
                          (skipSatisfy (\c -> c /= ' ' || c /= '\n' || c /= '\t' || c /= '\n'))
                    finalPos <- getPos
                    err $ InvalidNumberLiteral initialPos finalPos (firstBs <> invalidRest)
        )
        (pure $ Token (NUMBER n1) initialPos)

skipSatisfyAlphaNumeric :: ParserT st r e ()
skipSatisfyAlphaNumeric =
  skipFusedSatisfy
    (\c -> isLatinLetter c || isDigit c)
    isLetter
    isLetter
    isLetter

parseKeywAndIdentif :: ParserT st r e Token
parseKeywAndIdentif = do
  initialPos <- getPos
  toktype <- getTokType
  pure $ Token toktype initialPos
  where
    checkNext other =
      branch
        (lookahead skipSatisfyAlphaNumeric)
        (skipMany skipSatisfyAlphaNumeric >> pure (IDENTIFIER ""))
        $ pure other
    getTokType =
      $( switch
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
                skipSatisfy isLatinLetter
                skipMany skipSatisfyAlphaNumeric
                pure (IDENTIFIER "")
            |]
       )

-------------------------------------------------------------------------------------

godScan :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
godScan = do
  initialPos <- getPos
  toktype <- getTokType initialPos
  skipWhiteSpace
  pure $ Token toktype initialPos
  where
    scanNumber :: Pos -> ParserT (STMode s) (STRef s ScanErr) ScannerError TokenType
    scanNumber initialPos =
      parse
      where
        parse = do
          firstBs <- byteStringOf $ skipSome (skipSatisfy isDigit)

          let nBeforeDot =
                fst $
                  fromMaybe
                    (error "Reached the impossible: number parsing should be guaranteed")
                    (B.readInt firstBs)
          let n1 = fromIntegral nBeforeDot

          branch
            (skipSatisfy (== '.'))
            -- if there __IS__ a fractional part, we return the sum of both
            ( do
                dotPos <- getPos
                cmaybe <- optional (lookahead anyChar)
                case cmaybe of
                  Nothing -> err $ InvalidNumberLiteral initialPos dotPos (B.snoc firstBs '.')
                  Just ch ->
                    if isDigit ch
                      then do
                        secondBs <- byteStringOf $ skipSome (skipSatisfy isDigit)
                        let numAfterDot =
                              fst $
                                fromMaybe
                                  (error "Reached the impossible: number parsing should be guaranteed")
                                  (B.readInt secondBs)
                            n2 = fromIntegral numAfterDot / (10 ^ B.length secondBs)
                        pure $ NUMBER (n1 + n2)
                      else do
                        skipBack 1
                        invalidRest <-
                          byteStringOf $
                            skipMany
                              (skipSatisfy (\c -> c /= ' ' || c /= '\n' || c /= '\t' || c /= '\n'))
                        finalPos <- getPos
                        err $ InvalidNumberLiteral initialPos finalPos (firstBs <> invalidRest)
            )
            -- if there __IS NOT__ a fractional part, we return the integral part
            (pure $ NUMBER n1)

    scanString :: Pos -> ParserT (STMode s) (STRef s ScanErr) ScannerError TokenType
    scanString initPos = do
      bs <- byteStringOf $ skipMany (skipSatisfy (/= '"'))
      branch
        (skip 1) -- skip the (") character
        (pure (STRING bs))
        (err $ UnterminatedString initPos)

    scanIdentifier :: ParserT st r e TokenType
    scanIdentifier =
      IDENTIFIER
        <$> byteStringOf (skipSatisfy isLatinLetter >> skipMany skipSatisfyAlphaNumeric)
    checkNextMultipleCharToken :: TokenType -> ParserT st r e TokenType
    checkNextMultipleCharToken other =
      ( IDENTIFIER <$> byteStringOf (skipSome skipSatisfyAlphaNumeric)
      )
        <|> pure other
    getTokType initPos =
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
              -- operators

              "!=" -> pure BANG_EQUAL
              "=" -> pure EQUAL
              "==" -> pure EQUAL_EQUAL
              "<" -> pure LESS
              "<=" -> pure LESS_EQUAL
              ">" -> pure GREATER
              ">=" -> pure GREATER_EQUAL
              "/" -> pure SLASH
              "\"" -> scanString initPos
              -- longer lexemes (keywords)

              "and" -> checkNextMultipleCharToken AND
              "class" -> checkNextMultipleCharToken CLASS
              "else" -> checkNextMultipleCharToken ELSE
              "false" -> checkNextMultipleCharToken FALSE
              "for" -> checkNextMultipleCharToken FOR
              "fun" -> checkNextMultipleCharToken FUNN
              "if" -> checkNextMultipleCharToken IF
              "nil" -> checkNextMultipleCharToken NIL
              "or" -> checkNextMultipleCharToken OR
              "print" -> checkNextMultipleCharToken PRINT
              "return" -> checkNextMultipleCharToken RETURN
              "super" -> checkNextMultipleCharToken SUPER
              "this" -> checkNextMultipleCharToken THIS
              "true" -> checkNextMultipleCharToken TRUE
              "var" -> checkNextMultipleCharToken VAR
              "while" -> checkNextMultipleCharToken WHILE
              _ -> do
                scanIdentifier <|> scanNumber initPos
            |]
       )

testGod :: ByteString -> IO ()
testGod txt =
  case runST $ simpleScanParserU txt $ VBA.many godScan of
    OK (res :: Vector Token) _ bs -> do
      traverse_ print res
      putStrLn $ "Rest of BS: '" <> B.unpack bs <> "'"
    Err e -> putStrLn $ "Error: --" <> show e <> "--"
    Fail -> putStrLn "Unhandled failure'"
