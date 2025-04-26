{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Parser (runParse) where

import Control.Monad (void, when)
import Control.Monad.Except
  ( ExceptT
  , MonadError (throwError)
  , runExceptT
  , tryError
  )
import Control.Monad.RWS.CPS
  ( RWST
  , evalRWST
  )
import Control.Monad.State.Class (MonadState (get), modify')
import Data.ByteString.Char8 (ByteString)
import Data.Functor ((<&>))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Error qualified
import Expr
import Stmt qualified
import TokenType qualified
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB

data Parser = Parser
  { current :: !Int
  , tokens :: !(Vector TokenType.Token)
  }
  deriving (Show)

{- |
Removing newtypes, @ParserM r a@ is equivalent to:
  + @RWS r ParseException Parser (Either Error.ErrorPresent a)@
  + @RWST r ParseException Parser Identity (Either Error.ErrorPresent a)@
  + @r -> Parser -> Identity (Either Error.ErrorPresent a, Parser, ParseException)@
-}
type ParserM r a =
  ExceptT ParseException (RWST r Error.ErrorPresent Parser IO) a

-- type ParserM r a = RWST r [Error] Parser (Either Error) a

data ParseException = ParseException

runParse
  :: Vector TokenType.Token -> IO (Maybe (Vector Stmt.Stmt1), Error.ErrorPresent)
runParse tokens = do
  (r, w) <- (evalRWST . runExceptT) parse () initialParserState
  pure (either (const Nothing) Just r, w)
  where
    initialParserState = Parser {current = 0, tokens = tokens}

parse :: ParserM r (Vector Stmt.Stmt1)
parse = go VB.empty
  where
    go accum = do
      e <- not <$> isAtEnd
      if e
        then do
          next <- declaration
          case next of
            Just n -> go (accum <> VB.singleton n)
            Nothing -> go accum
        else pure $ VB.build accum

varDeclaration :: ParserM r Stmt.Stmt1
varDeclaration = do
  name <- consume TokenType.IDENTIFIER "Expect variable name."
  m <- match [TokenType.EQUAL]
  initializer <- if m then Just <$> expression else pure Nothing
  consume TokenType.SEMICOLON "Expect ';' after variable declaration."
  pure $ Stmt.SVar name initializer

{-# INLINE declaration #-}
declaration :: ParserM r (Maybe Stmt.Stmt1)
declaration = do
  e <- tryError $ do
    safePeek >>= \case
      Just (TokenType.Token TokenType.CLASS _ _) -> advance >> classDeclaration
      Just (TokenType.Token TokenType.FUNN _ _) -> advance >> functionDeclaration
      Just (TokenType.Token TokenType.VAR _ _) -> advance >> varDeclaration
      _ -> statement
  case e of
    Left _ -> synchronize >> pure Nothing
    Right v -> pure $ Just v

{-# INLINE classDeclaration #-}
classDeclaration :: ParserM r Stmt.Stmt1
classDeclaration = do
  name <- consume TokenType.IDENTIFIER "Expect class name."
  superclass <-
    safePeek >>= \case
      Just (TokenType.Token TokenType.LESS _ _) ->
        advance
          >> Just . (,()) <$> consume TokenType.IDENTIFIER "Expect superclass name."
      _ -> pure Nothing
  consume TokenType.LEFT_BRACE "Expect '{' before class body.."
  methods <- getMethods VB.empty
  consume TokenType.RIGHT_BRACE "Expect '}' after class body."
  pure $ Stmt.SClass name superclass methods
  where
    getMethods :: VB.Builder Stmt.FunctionH1 -> ParserM r (Vector Stmt.FunctionH1)
    getMethods accum = do
      not <$> check TokenType.RIGHT_BRACE >>= \case
        -- if we havent found RIGHT_BRACE, we try parsing class methods
        True -> do
          nextMethod <- function "method"
          getMethods (accum <> VB.singleton nextMethod)
        False -> pure $ VB.build accum

{-# INLINE functionDeclaration #-}
functionDeclaration :: ParserM r Stmt.Stmt1
functionDeclaration = Stmt.SFunction <$> function "function"

{-# INLINEABLE function #-}
function :: ByteString -> ParserM r Stmt.FunctionH1
function kind = do
  name <- consume TokenType.IDENTIFIER $ "Expect " <> kind <> " name."
  consume TokenType.LEFT_PAREN $ "Expect '(' after " <> kind <> " name."
  parameters <-
    not <$> check TokenType.RIGHT_PAREN >>= \case
      -- if we haven't found yet a RIGHT_PAREN (ex. 'fun name(1, arg1)')
      True -> getParams VB.empty
      -- if we have found a RIGHT_PAREN (ex. 'fun name()')
      False -> pure V.empty
  consume TokenType.RIGHT_PAREN "Expect ')' after parameters."
  consume TokenType.LEFT_BRACE $ "Expect '{' before " <> kind <> " body."
  Stmt.FFunctionH name parameters <$> block
  where
    getParams :: VB.Builder TokenType.Token -> ParserM r (Vector TokenType.Token)
    getParams accum = do
      when (VB.size accum >= 255) $ do
        peek >>= \tok -> void $ getAndReportParserError tok "Can't have more than 255 parameters."
      nextParamName <- consume TokenType.IDENTIFIER "Expect parameter name."
      match [TokenType.COMMA] >>= \case
        True -> getParams (accum <> VB.singleton nextParamName)
        False -> pure $ VB.build (accum <> VB.singleton nextParamName)

statement :: ParserM r Stmt.Stmt1
statement =
  safePeek >>= \case
    Just (TokenType.Token TokenType.FOR _ _) -> advance >> forStatement
    Just (TokenType.Token TokenType.IF _ _) -> advance >> ifStatement
    Just (TokenType.Token TokenType.PRINT _ _) -> advance >> printStatement
    Just (TokenType.Token TokenType.RETURN _ _) -> advance >> returnStatement
    Just (TokenType.Token TokenType.WHILE _ _) -> advance >> whileStatement
    Just (TokenType.Token TokenType.LEFT_BRACE _ _) -> advance >> (Stmt.SBlock <$> block)
    _ -> expressionStatement

returnStatement :: ParserM r Stmt.Stmt1
returnStatement = do
  keyword <- previous
  value <-
    not <$> check TokenType.SEMICOLON >>= \case
      -- if we do not find a semicolon immediately, there is an expression
      True -> Just <$> expression
      False -> pure Nothing
  consume TokenType.SEMICOLON "Expect ';' after return value."
  pure $ Stmt.SReturn keyword value

forStatement :: ParserM r Stmt.Stmt1
forStatement = do
  consume TokenType.LEFT_PAREN "Expect '(' after 'for'."
  initializer <-
    safePeek >>= \case
      Just (TokenType.Token TokenType.SEMICOLON _ _) -> advance >> pure Nothing
      Just (TokenType.Token TokenType.VAR _ _) -> advance >> Just <$> varDeclaration
      _ -> Just <$> expressionStatement
  condition <-
    not <$> check TokenType.SEMICOLON >>= \case
      True -> expression
      -- if there's no condition, it's an infinite loop
      False -> pure $ Expr.ELiteral $ LBool True
  consume TokenType.SEMICOLON "Expect ';' after loop condition."
  increment <-
    not <$> check TokenType.RIGHT_PAREN >>= \case
      True -> Just <$> expression
      False -> pure Nothing
  consume TokenType.RIGHT_PAREN "Expect ')' after for clauses."
  whileStmtBody <-
    statement <&> \forStmtBody -> case increment of
      -- if there's an increment in the 'for', add it to the end of the 'while' body
      Just inc -> Stmt.SBlock $ V.fromList [forStmtBody, Stmt.SExpression inc]
      Nothing -> forStmtBody
  -- while statement is built from the condition and the desugared for body & increment
  let whileStmt = Stmt.SWhile condition whileStmtBody
  -- finally, if there is an initializer (ex. 'for (var a = 1...)'), we prefix
  -- it to the desugared while loop
  let finishedForStmt = case initializer of
        Just ini -> Stmt.SBlock $ V.fromList [ini, whileStmt]
        Nothing -> whileStmt
  pure finishedForStmt

whileStatement :: ParserM r Stmt.Stmt1
whileStatement = do
  consume TokenType.LEFT_PAREN "Expect '(' after 'while'."
  condition <- expression
  consume TokenType.RIGHT_PAREN "Expect ')' after condition."
  Stmt.SWhile condition <$> statement

ifStatement :: ParserM r Stmt.Stmt1
ifStatement = do
  consume TokenType.LEFT_PAREN "Expect '(' after 'if'."
  condition <- expression
  consume TokenType.RIGHT_PAREN "Expect ')' after if condition."
  thenBranch <- statement
  elseBranch <-
    match [TokenType.ELSE] >>= \case
      True -> Just <$> statement
      False -> pure Nothing
  pure $ Stmt.SIf condition thenBranch elseBranch

block :: ParserM r (Vector Stmt.Stmt1)
block = go VB.empty <* consume TokenType.RIGHT_BRACE "Expect '}' after block."
  where
    go accum = do
      cond <- liftA2 (&&) (not <$> check TokenType.RIGHT_BRACE) (not <$> isAtEnd)
      if cond
        then do
          next <- declaration
          case next of
            Just n -> go (accum <> VB.singleton n)
            Nothing -> go accum
        else pure (VB.build accum)

printStatement :: ParserM r Stmt.Stmt1
printStatement = do
  value <- expression
  consume TokenType.SEMICOLON "Expect ';' after value."
  pure $ Stmt.SPrint value

expressionStatement :: ParserM r Stmt.Stmt1
expressionStatement = do
  expr <- expression
  consume TokenType.SEMICOLON "Expect ';' after expression."
  pure $ Stmt.SExpression expr

expression :: ParserM r Expr1
expression = assignment

assignment :: ParserM r Expr1
assignment = do
  expr <- orP
  m <- match [TokenType.EQUAL]
  if m
    then do
      equals <- previous
      value <- assignment
      case expr of
        Expr.EVariable name _ -> pure $ Expr.EAssign name value ()
        Expr.EGet object name -> pure $ Expr.ESet object name value
        _ -> do
          -- the error is reported, but does not need to synchronize, hence why there's no `throwError`
          getAndReportParserError equals "Invalid assignment target."
          pure expr
    else pure expr

orP :: ParserM r Expr1
orP = andP >>= whileParseELogical andP [TokenType.OR]

andP :: ParserM r Expr1
andP = equality >>= whileParseELogical equality [TokenType.AND]

equality :: ParserM r Expr1
equality =
  comparison
    >>= whileParseEBinary comparison [TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL]

comparison :: ParserM r Expr1
comparison =
  term
    >>= whileParseEBinary
      term
      [ TokenType.GREATER
      , TokenType.GREATER_EQUAL
      , TokenType.LESS
      , TokenType.LESS_EQUAL
      ]

term :: ParserM r Expr1
term = factor >>= whileParseEBinary factor [TokenType.MINUS, TokenType.PLUS]

factor :: ParserM r Expr1
factor = unary >>= whileParseEBinary unary [TokenType.SLASH, TokenType.STAR]

unary :: ParserM r Expr1
unary = do
  match [TokenType.BANG, TokenType.MINUS] >>= \m ->
    if m
      then liftA2 EUnary previous unary
      else
        call

call :: ParserM r Expr1
call = do
  expr <- primary
  whileTrue expr
  where
    whileTrue :: Expr1 -> ParserM r Expr1
    whileTrue prev = do
      safePeek >>= \case
        -- if we find a LEFT_PAREN after a primary expr, this is a function call
        Just (TokenType.Token TokenType.LEFT_PAREN _ _) ->
          advance >> finishCall prev >>= whileTrue
        Just (TokenType.Token TokenType.DOT _ _) -> do
          advance
          name <- consume TokenType.IDENTIFIER "Expect property name after '.'."
          whileTrue $ EGet prev name
        _ ->
          pure prev
    finishCall :: Expr1 -> ParserM r Expr1
    finishCall callee = do
      arguments <-
        not <$> check TokenType.RIGHT_PAREN >>= \case
          -- if we have'nt found yet a RIGHT_PAREN
          True -> getArgs 0 VB.empty
          -- if we have found a RIGHT_PAREN
          False -> pure V.empty
      paren <- consume TokenType.RIGHT_PAREN "Expect ')' after arguments."
      pure $ ECall callee paren arguments
      where
        getArgs :: Int -> VB.Builder Expr1 -> ParserM r (Vector Expr1)
        getArgs argCount accum = do
          when (argCount >= 255) $ do
            peek >>= \tok -> void $ getAndReportParserError tok "Can't have more than 255 arguments."
          nextArg <- expression
          match [TokenType.COMMA] >>= \case
            True -> getArgs (argCount + 1) (accum <> VB.singleton nextArg)
            False -> pure $ VB.build (accum <> VB.singleton nextArg)

primary :: ParserM r Expr1
primary = do
  t <- safePeek
  case t of
    Just (TokenType.Token TokenType.FALSE _ _) -> advance >> pure (ELiteral $ LBool False)
    Just (TokenType.Token TokenType.TRUE _ _) -> advance >> pure (ELiteral $ LBool True)
    Just (TokenType.Token TokenType.NIL _ _) -> advance >> pure (ELiteral LNil)
    Just (TokenType.Token (TokenType.NUMBER lit) _ _) -> advance >> pure (ELiteral $ LNumber lit)
    Just (TokenType.Token (TokenType.STRING lit) _ _) -> advance >> pure (ELiteral $ LString lit)
    Just keyword@(TokenType.Token TokenType.SUPER _ _) -> do
      advance >> consume TokenType.DOT "Expect '.' after 'super'."
      method <- consume TokenType.IDENTIFIER "Expect superclass method name."
      pure $ ESuper keyword () method
    Just tok@(TokenType.Token TokenType.THIS _ _) -> advance >> pure (EThis tok ())
    Just tok@(TokenType.Token TokenType.IDENTIFIER _ _) -> advance >> pure (EVariable tok ())
    Just (TokenType.Token TokenType.LEFT_PAREN _ _) ->
      advance >> do
        expr <- expression
        consume TokenType.RIGHT_PAREN "Expect ')' after expression."
        pure $ EGrouping expr
    _ -> do
      p <- peek
      getAndReportParserError p "Expect expression." >>= throwError

safePeek :: ParserM r (Maybe TokenType.Token)
safePeek = do
  c <- not <$> isAtEnd
  if c then Just <$> peek else pure Nothing

{-# INLINE whileParse #-}
whileParse
  :: (Expr1 -> TokenType.Token -> Expr1 -> Expr1)
  -> ParserM r Expr1
  -> [TokenType.TokenType]
  -> Expr1
  -> ParserM r Expr1
whileParse constr = go
  where
    go inner matchlist eAccum = do
      m <- match matchlist
      if m
        then do
          operator <- previous
          right <- inner
          go inner matchlist $ constr eAccum operator right
        else
          pure eAccum

whileParseEBinary
  :: ParserM r Expr1 -> [TokenType.TokenType] -> Expr1 -> ParserM r Expr1
whileParseEBinary = whileParse EBinary

whileParseELogical
  :: ParserM r Expr1 -> [TokenType.TokenType] -> Expr1 -> ParserM r Expr1
whileParseELogical = whileParse ELogical

synchronize :: ParserM r ()
synchronize = advance >> go
  where
    go :: ParserM r ()
    go = do
      e <- not <$> isAtEnd
      if e
        then do
          t <- previous
          p <- peek
          if t.ttype == TokenType.SEMICOLON
            then pure ()
            else case p.ttype of
              TokenType.CLASS -> pure ()
              TokenType.FUNN -> pure ()
              TokenType.VAR -> pure ()
              TokenType.FOR -> pure ()
              TokenType.IF -> pure ()
              TokenType.WHILE -> pure ()
              TokenType.PRINT -> pure ()
              TokenType.RETURN -> pure ()
              _ -> advance >> go
        else do
          pure ()

consume :: TokenType.TokenType -> ByteString -> ParserM r TokenType.Token
consume ttype message =
  check ttype >>= \c ->
    if c
      then advance
      else
        peek >>= (`getAndReportParserError` message) >>= throwError

getAndReportParserError
  :: TokenType.Token -> ByteString -> ParserM r ParseException
getAndReportParserError token message = do
  Error.parseError token message
  pure ParseException

match :: [TokenType.TokenType] -> ParserM r Bool
match = \case
  [] -> pure False
  (ttype : ttypes) ->
    check ttype >>= \c ->
      if c
        then advance >> pure True
        else match ttypes

check :: TokenType.TokenType -> ParserM r Bool
check ttype =
  isAtEnd >>= \c ->
    if c
      then pure False
      else do
        p <- peek
        pure (p.ttype == ttype)

advance :: ParserM r TokenType.Token
advance = do
  c <- not <$> isAtEnd
  when c $ modify' $ \pr -> pr {current = pr.current + 1}
  previous

isAtEnd :: ParserM r Bool
isAtEnd = (\t -> t.ttype == TokenType.EOF) <$> peek

-- TODO: unsafeIndex ?
peek :: ParserM r TokenType.Token
peek = get <&> \pr -> pr.tokens V.! pr.current

previous :: ParserM r TokenType.Token
previous = get <&> \pr -> pr.tokens V.! (pr.current - 1)
