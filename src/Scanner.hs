{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , untilEnd
  , getScannerErrorPos
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.ST
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char (isLetter)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import FlatParse.Common.Parser (STMode)
import FlatParse.Stateful hiding ((<|>))
import Token
import VectorBuilder.Builder qualified as VBB
import VectorBuilder.Vector qualified as VBV

data ScannerError
  = UnexpectedCharacter
      !Pos
      -- ^ The position of the unexpected character
      !(Maybe (Int, Int))
      -- ^ The line and colum of the unexpected character
      !Char
      -- ^ The unexpected character itself
  | UnterminatedString
      !Pos
      -- ^ The position of the start of the unterminated string
      !(Maybe (Int, Int))
      -- ^ The line and colum of the start of the unterminated string
  | InvalidNumberLiteral
      !(Pos, Pos)
      -- ^ The position of the start and end of the invalid number literal
      !(Maybe ((Int, Int), (Int, Int)))
      -- ^ The line and colum of the start and end of the invalid number literal
      !ByteString
      -- ^ The bytestring of the infalid number literal itself
  | UnexpectedScannerFailure
      !Pos
      -- ^  The position where the failure was reported. This should never happen though, becasue
      --  the last alternative for failing to parse anything, is to throw an error and continue.
      !(Maybe (Int, Int))
  deriving (Show)

getScannerErrorPos :: ScannerError -> [Pos]
getScannerErrorPos = \case
  UnexpectedCharacter p _ _ -> [p]
  UnterminatedString p _ -> [p]
  InvalidNumberLiteral (b, e) _ _ -> [b, e]
  UnexpectedScannerFailure p _ -> [p]

data CodeError
  = CUnexpectedCharacter !Char !(Int, Int)
  | CUnterminatedString !(Int, Int)
  | CInvalidNumberLiteral !(Int, Int) !ByteString
  deriving (Show)

newtype ScanErr = ErrVec (Vector ScannerError)
  deriving (Show)

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
      UnexpectedCharacter pos _ ch ->
        Left $ CUnexpectedCharacter ch (errorGetLineCol pos)
      UnterminatedString pos _ ->
        Left $ CUnterminatedString (errorGetLineCol pos)
      InvalidNumberLiteral (pos, _) _ errBs ->
        Left $ CInvalidNumberLiteral (errorGetLineCol pos) errBs
      _ -> error "Failure should never propagate here"
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
          _ -> pure ()
        |]
   )
  where
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
      UnexpectedCharacter unexpectedCharPos _ _ -> setPos unexpectedCharPos >> skipMany (skip 1) -- skipMany to not fail if we are at end
      UnterminatedString _ _ -> setPos endPos
      InvalidNumberLiteral (_, invalidEndPos) _ _ -> setPos invalidEndPos
      er -> err er

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
              Nothing -> err $ InvalidNumberLiteral (initialPos, dotPos) Nothing (B.snoc firstBs '.')
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
                    err $
                      InvalidNumberLiteral (initialPos, finalPos) Nothing (firstBs <> invalidRest)
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
    (err $ UnterminatedString initPos Nothing)

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

scanSingleToken :: ParserT (STMode s) (STRef s ScanErr) ScannerError Token
scanSingleToken = do
  initialPos <- getPos
  toktype <- getTokType initialPos
  pure $ Token toktype initialPos
  where
    getTokType initPos =
      scanOperatorOrSimple initPos
        <|> scanKeywordAndIdentifier initPos
        <|> ifNothingMatchesReportError

ifNothingMatchesReportError :: ParserT st r ScannerError b
ifNothingMatchesReportError = do
  ch <- lookahead anyChar
  pos <- getPos
  err $ UnexpectedCharacter pos Nothing ch

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
                reportError $ UnexpectedScannerFailure pos Nothing
                pure carry
            )
        whenError a = do
          handleErr a
          branch
            eof
            (pure carry)
            (go carry parser)
