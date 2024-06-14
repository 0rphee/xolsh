{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Scanner
  ( CodeError (..)
  , ScannerError (..)
  , runScanFile
  , simpleScanParser
  , scanOperatorOrSimple
  , scanKeywordAndIdentifier
  , scanNumber
  , scanSingleToken
  , scanStringLiteral
  , skipWhiteSpace
  , testGod
  , printErrs
  , ppPrintErr
  , untilEnd
  )
where

import Control.Applicative (Alternative (..))
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
import FlatParse.Stateful hiding ((<|>))
import Token
import VectorBuilder.Alternative qualified as VBA
import VectorBuilder.Builder qualified as VBB
import VectorBuilder.Vector qualified as VBV

data ScannerError
  = UnexpectedCharacter
      !Pos
      -- ^ The position of the unexpected character
      !Char
      -- ^ The unexpected character itself
  | -- | The position of the start of the unterminated string
    UnterminatedString
      !Pos
  | InvalidNumberLiteral
      !Pos
      -- ^ The position of the start of the invalid number literal
      !Pos
      -- ^ The position of the end of the invalid number literal
      !ByteString
      -- ^ The bytestring of the infalid number literal itself
  deriving (Show)

data CodeError
  = CUnexpectedCharacter !Char !(Int, Int)
  | CUnterminatedString !(Int, Int)
  | CInvalidNumberLiteral !(Int, Int) !ByteString
  deriving (Show)

newtype ScanErr = ErrVec (Vector ScannerError)
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

simpleScanParser
  :: ByteString
  -> ParserST s (STRef s ScanErr) e a
  -> ST s (Vector ScannerError, Result e a)
simpleScanParser bs p = do
  stref <- newSTRef $ ErrVec V.empty
  res <- runParserST p stref 0 bs
  (ErrVec _errVec) <- readSTRef stref
  pure (_errVec, res)

runScanFile
  :: ByteString
  -> Either CodeError (Vector ScannerError, Vector Token, ByteString)
runScanFile bs = runST $ do
  stRef <- newSTRef $ ErrVec V.empty

  res <- runParserST (untilEnd scanSingleToken) stRef 0 bs

  (ErrVec errVec) <- readSTRef stRef
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

reportError :: ScannerError -> ParserT (STMode s) (STRef s ScanErr) e ()
reportError e = do
  stref <- ask
  liftST $ appendSTRefSCanErrors stref e
  where
    appendSTRefSCanErrors :: STRef s ScanErr -> ScannerError -> ST s ()
    appendSTRefSCanErrors stref er = modifySTRef' stref (apndScanningErrors er)
      where
        apndScanningErrors codeError (ErrVec v) = ErrVec $ V.snoc v codeError

skipWhiteSpace :: ParserT (STMode s) (STRef s ScanErr) ScannerError ()
skipWhiteSpace =
  $( switch
      [|
        case _ of
          " " -> skipWhiteSpace
          "\r" -> skipWhiteSpace
          "\t" -> skipWhiteSpace
          "\n" -> skipWhiteSpace
          "//" -> skipLineComment
          _ -> do
            branch
              (fails $ lookahead scanSingleToken)
              -- if scanSingleToken fails, then we report an error
              ( do
                  pos <- getPos
                  ch <- anyChar
                  reportError $ UnexpectedCharacter pos ch
                  skipWhiteSpace
              )
              -- if scanSingleToken doesnt fail then we continue
              (pure ())
        |]
   )
  where
    withThree
      :: ParserT st r e a
      -> ParserT st r e b
      -> ParserT st r e b
      -> (e -> ParserT st r e b)
      -> ParserT st r e b
    withThree pa pt pf pwe = ParserT \fp !r eob s n st -> case runParserT# pa fp r eob s n st of
      OK# st' _ s n' -> runParserT# pt fp r eob s n' st'
      Fail# st' -> runParserT# pf fp r eob s n st'
      Err# st' e -> runParserT# (pwe e) fp r eob s n st'

    skipFailure =
      withThree
        (fails $ lookahead scanSingleToken)
        -- if scanSingleToken fails, then we report an error
        ( do
            pos <- getPos
            ch <- anyChar -- this advances the parser
            reportError $ UnexpectedCharacter pos ch
            skipWhiteSpace
        )
        -- if scanSingleToken doesnt fail then we continue consuming characters?
        (pure ())
        -- if scanSingleToken throws an error then
        handleErr

    skipLineComment :: ParserT (STMode s) (STRef s ScanErr) ScannerError ()
    skipLineComment =
      branch eof (pure ()) $
        withOption
          anyWord8
          ( \case
              10 -> skipWhiteSpace -- '\n'
              _ -> skipLineComment
          )
          (pure ())

handleErr
  :: ScannerError -> ParserT (STMode s) (STRef s ScanErr) ScannerError ()
handleErr e = do
  reportError e
  moveToCorrespondingPlace
  where
    moveToCorrespondingPlace = case e of
      UnexpectedCharacter unexpectedCharPos _ -> setPos unexpectedCharPos >> skipMany (skip 1) -- skipMany to not fail if we are at end
      UnterminatedString _ -> setPos endPos
      InvalidNumberLiteral _ invalidEndPos _ -> setPos invalidEndPos

skipSatisfyAlphaNumeric :: ParserT st r e ()
skipSatisfyAlphaNumeric =
  skipFusedSatisfy
    (\c -> isLatinLetter c || isDigit c)
    isLetter
    isLetter
    isLetter

-------------------------------------------------------------------------------------
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

scanStringLiteral
  :: Pos -> ParserT (STMode s) (STRef s ScanErr) ScannerError TokenType
scanStringLiteral initPos = do
  bs <- byteStringOf $ skipMany (skipSatisfy (/= '"'))
  branch
    (skip 1) -- skip the (") character
    (pure (STRING bs))
    (err $ UnterminatedString initPos)

{- |
Scans:

- simple characters: @'('@, @')'@, @'{'@, @'}'@, @','@, @'.'@, @'/'@, @'"'@ (string literals)

- operators: @'-'@, @'+'@, @';'@, @'*'@, @'!'@, @'!='@, @'='@, @'=='@, @'<'@, @'<='@, @'>'@, @'>='@
-}
scanOperatorOrSimple
  :: Pos -> ParserT (STMode s) (STRef s ScanErr) ScannerError TokenType
scanOperatorOrSimple initPos =
  $( switch
      [|
        case _ of
          -- simple tokens
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
          "\"" -> scanStringLiteral initPos
        |]
   )

scanKeywordAndIdentifier
  :: Pos
  -> ParserT (STMode s) (STRef s ScanErr) ScannerError TokenType
scanKeywordAndIdentifier initPos =
  let checkNextMultipleCharToken :: TokenType -> ParserT st r e TokenType
      checkNextMultipleCharToken other =
        ( IDENTIFIER <$> byteStringOf (skipSome skipSatisfyAlphaNumeric)
        )
          <|> pure other
      scanIdentifier :: ParserT st r e TokenType
      scanIdentifier =
        IDENTIFIER
          <$> byteStringOf (skipSatisfy isLatinLetter >> skipMany skipSatisfyAlphaNumeric)
   in $( switch
          [|
            case _ of
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

scanSingleToken :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
scanSingleToken = do
  initialPos <- getPos
  toktype <- getTokType initialPos
  pure $ Token toktype initialPos
  where
    getTokType initPos = scanOperatorOrSimple initPos <|> scanKeywordAndIdentifier initPos

untilEnd
  :: ParserT (STMode s) (STRef s ScanErr) ScannerError elem
  -> ParserT (STMode s) (STRef s ScanErr) ScannerError (Vector elem)
untilEnd p = VBV.build <$> go VBB.empty (skipWhiteSpace >> p)
  where
    go
      :: VBB.Builder a
      -> ParserT (STMode s) (STRef s ScanErr) ScannerError a
      -> ParserT (STMode s) (STRef s ScanErr) ScannerError (VBB.Builder a)
    go carry parser@(ParserT initial) =
      ParserT \fp !r eob s n st ->
        case initial fp r eob s n st of
          OK# st' a s' n' -> runParserT# (whenSuccess a) fp r eob s' n' st'
          Fail# st' -> runParserT# whenFailure fp r eob s n st'
          Err# st' e -> runParserT# (whenError e) fp r eob s n st'
      where
        whenSuccess res = go (carry <> VBB.singleton res) parser
        whenFailure = do
          branch
            eof
            (pure carry)
            ( do
                pos <- getPos
                ch <- lookahead anyChar
                unsafeLiftIO $ putStrLn $ "pos " <> show pos <> " char " <> [ch]
                skip 1 >> go carry parser
            )
        whenError a = do
          handleErr a
          branch
            eof
            (pure carry)
            (go carry parser)

testGod :: ByteString -> IO ()
testGod txt =
  case runST $ simpleScanParser txt (untilEnd scanSingleToken) of
    (evec, OK (res :: Vector Token) _ bs) -> do
      traverse_ print res
      printErrs txt evec
      putStrLn $ "Rest of BS: '" <> B.unpack bs <> "'"
    (evec, Err e) -> do
      putStrLn $ "Error: --" <> show e <> "--"
      printErrs txt evec
    (evec, Fail) -> do
      putStrLn "Unhandled failure'"
      printErrs txt evec
