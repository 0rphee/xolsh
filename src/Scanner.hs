{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Scanner
  ( CodeError (..)
  , ScannerError (..)
  , ScanLocation (..)
  , runScanOnString
  , simpleScanParser
  , scanOperatorOrSimple
  , scanKeywordAndIdentifier
  , scanNumber
  , scanSingleToken
  , scanStringLiteral
  , skipWhiteSpace
  , untilEnd
  , getScannerErrorPos
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.ST
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char (isLetter)
import Data.Maybe (fromMaybe)
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import FlatParse.Common.Parser (STMode)
import FlatParse.Stateful hiding ((<|>))
import Token
import VectorBuilder.Builder qualified as VBB
import VectorBuilder.Vector qualified as VBV

data ScanLocation = AsPos | AsLineCol

type family GetScanLocationType (a :: ScanLocation) = res | res -> a where
  GetScanLocationType AsPos = Pos
  GetScanLocationType AsLineCol = (Int, Int)

data ScannerError (a :: ScanLocation) where
  UnexpectedCharacter
    :: !(GetScanLocationType a)
    -- ^ The position/lineCol of the unexpected character
    -> !Char
    -- ^ The unexpected character itself
    -> ScannerError a
  UnterminatedString
    :: !(GetScanLocationType a)
    -- ^ The position of the start of the unterminated string
    -> ScannerError a
  InvalidNumberLiteral
    :: !(GetScanLocationType a, GetScanLocationType a)
    -- ^ The position of the start and end of the invalid number literal
    -- -> !ByteString
    -- -- ^ The bytestring of the infalid number literal itself
    -> ScannerError a
  UnexpectedScannerFailure
    :: !(GetScanLocationType a)
    -- ^  The position where the failure was reported. This should never happen though, becasue
    --  the last alternative for failing to parse anything, is to throw an error and continue.
    -> ScannerError a

deriving instance Show (GetScanLocationType a) => Show (ScannerError a)

getScannerErrorPos :: ScannerError AsPos -> [Pos]
getScannerErrorPos = \case
  UnexpectedCharacter p _ -> [p]
  UnterminatedString p -> [p]
  InvalidNumberLiteral (b, e) -> [b, e]
  UnexpectedScannerFailure p -> [p]

data CodeError
  = CUnexpectedCharacter !Char !(Int, Int)
  | CUnterminatedString !(Int, Int)
  | CInvalidNumberLiteral !(Int, Int) !ByteString
  deriving (Show)

newtype ScanErr = ErrVec (Vector (ScannerError AsPos))
  deriving (Show)

simpleScanParser
  :: ByteString
  -> ParserST s (STRef s ScanErr) e a
  -> ST s (Vector (ScannerError AsPos), Result e a)
simpleScanParser bs p = do
  stref <- newSTRef $ ErrVec V.empty
  res <- runParserST p stref 0 bs
  (ErrVec _errVec) <- readSTRef stref
  pure (_errVec, res)

runScanOnString
  :: ByteString
  -> (Vector (ScannerError AsPos), Maybe (Vector Token, ByteString))
runScanOnString bs = runST $ do
  stRef <- newSTRef $ ErrVec V.empty

  res <- runParserST (untilEnd scanSingleToken) stRef 0 bs

  (ErrVec errVec) <- readSTRef stRef
  pure $ case res of
    OK vec _ restOfBs -> (errVec, Just (vec, restOfBs))
    Err e -> (V.snoc errVec e, Nothing)
    Fail -> (errVec, Nothing) -- though failure should never propagate here

reportError
  :: ScannerError AsPos -> ParserT (STMode s) (STRef s ScanErr) e ()
reportError e = do
  stref <- ask
  liftST $ appendSTRefSCanErrors stref e
  where
    appendSTRefSCanErrors :: STRef s ScanErr -> ScannerError AsPos -> ST s ()
    appendSTRefSCanErrors stref er = modifySTRef' stref (apndScanningErrors er)
      where
        apndScanningErrors codeError (ErrVec v) = ErrVec $ V.snoc v codeError

skipWhiteSpace
  :: ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) ()
skipWhiteSpace =
  $( switch
      [|
        case _ of
          " " -> skipWhiteSpace
          "\r" -> skipWhiteSpace
          "\t" -> skipWhiteSpace
          "\n" -> skipWhiteSpace
          "//" -> skipLineComment
          _ -> pure ()
        |]
   )
  where
    skipLineComment :: ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) ()
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
  :: ScannerError AsPos
  -> ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) ()
handleErr e = do
  reportError e
  moveToCorrespondingPlace
  where
    moveToCorrespondingPlace = case e of
      UnexpectedCharacter unexpectedCharPos _ -> setPos unexpectedCharPos >> skipMany (skip 1) -- skipMany to not fail if we are at end
      UnterminatedString _ -> setPos endPos
      InvalidNumberLiteral (_, invalidEndPos) -> setPos invalidEndPos
      er -> err er

skipSatisfyAlphaNumeric :: ParserT st r e ()
skipSatisfyAlphaNumeric =
  skipFusedSatisfy
    (\c -> isLatinLetter c || isDigit c)
    isLetter
    isLetter
    isLetter

-------------------------------------------------------------------------------------
scanNumber
  :: Pos -> ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) TokenType
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
              Nothing -> err $ InvalidNumberLiteral (initialPos, dotPos)
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
                    skipMany
                      (skipSatisfy (\c -> c /= ' ' || c /= '\n' || c /= '\t' || c /= '\n'))
                    finalPos <- getPos
                    err $
                      InvalidNumberLiteral (initialPos, finalPos)
        )
        -- if there __IS NOT__ a fractional part, we return the integral part
        (pure $ NUMBER n1)

scanStringLiteral
  :: Pos -> ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) TokenType
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
  :: Pos -> ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) TokenType
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
          "." ->
            branch
              (skipSome (skipSatisfy isDigit))
              ( do
                  ePos <- getPos
                  err $ InvalidNumberLiteral (initPos, ePos)
              )
              (pure DOT)
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
  -> ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) TokenType
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

scanSingleToken
  :: ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) Token
scanSingleToken = do
  initialPos <- getPos
  toktype <- getTokType initialPos
  pure $ Token toktype initialPos
  where
    getTokType initPos =
      scanKeywordAndIdentifier initPos
        <|> scanOperatorOrSimple initPos
        <|> ifNothingMatchesReportError

    ifNothingMatchesReportError :: ParserT st r (ScannerError AsPos) b
    ifNothingMatchesReportError = do
      ch <- lookahead anyChar
      pos <- getPos
      err $ UnexpectedCharacter pos ch

untilEnd
  :: ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) elem
  -> ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) (Vector elem)
untilEnd p = VBV.build <$> go VBB.empty (skipWhiteSpace >> p)
  where
    go
      :: VBB.Builder a
      -> ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) a
      -> ParserT (STMode s) (STRef s ScanErr) (ScannerError AsPos) (VBB.Builder a)
    go carry parser@(ParserT initial) =
      ParserT \fp !r eob s n st ->
        case initial fp r eob s n st of
          OK# st' a s' n' -> runParserT# (whenSuccess a) fp r eob s' n' st'
          Fail# st' -> runParserT# whenFailure fp r eob s n st'
          Err# st' e -> runParserT# (whenError e) fp r eob s n st'
      where
        whenSuccess res = go (carry <> VBB.singleton res) parser

        -- if we failed, even with ifNothingMatchesReportError, it means we
        -- have gotten to the eof while skipping whitespace, and we should finish
        whenFailure = do
          pure carry
        whenError a = do
          handleErr a
          branch
            eof
            (pure carry)
            (go carry parser)
