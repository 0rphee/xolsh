{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Parser (runParse) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Data.ByteString.Char8 (ByteString)
import Data.Functor ((<&>))
import Error qualified
import Expr
import Stmt qualified
import TokenType (Token (..), TokenType (..))

data Parser = Parser
  { current :: !Int
  , tokens :: ![Token]
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

runParse :: [Token] -> IO (Maybe [Stmt.Stmt], Error.ErrorPresent)
runParse tokens = do
  (r, w) <- (evalRWST . runExceptT) parse () initialParserState
  pure (either (const Nothing) Just r, w)
  where
    initialParserState = Parser {current = 0, tokens = tokens}

parse :: ParserM r [Stmt.Stmt]
parse = reverse <$> go []
  where
    go accum = do
      e <- not <$> isAtEnd
      if e
        then do
          next <- declaration
          case next of
            Just n -> go (n : accum)
            Nothing -> go accum
        else pure accum

varDeclaration :: ParserM r Stmt.Stmt
varDeclaration = do
  name <- consume IDENTIFIER "Expect variable name."
  m <- match [EQUAL]
  initializer <- if m then Just <$> expression else pure Nothing
  consume SEMICOLON "Expect ';' after variable declaration."
  pure $ Stmt.SVar name initializer

declaration :: ParserM r (Maybe Stmt.Stmt)
declaration = do
  e <- tryError $ do
    m <- match [VAR]
    if m
      then varDeclaration
      else statement
  case e of
    Left _ -> synchronize >> pure Nothing
    Right v -> pure $ Just v

statement :: ParserM r Stmt.Stmt
statement =
  safePeek >>= \case
    Just (Token FOR _ _) -> advance >> forStatement
    Just (Token IF _ _) -> advance >> ifStatement
    Just (Token PRINT _ _) -> advance >> printStatement
    Just (Token WHILE _ _) -> advance >> whileStatement
    Just (Token LEFT_BRACE _ _) -> advance >> (Stmt.SBlock <$> block)
    _ -> expressionStatement

forStatement :: ParserM r Stmt.Stmt
forStatement = do
  consume LEFT_PAREN "Expect '(' after 'for'."
  initializer <-
    safePeek >>= \case
      Just (Token SEMICOLON _ _) -> advance >> pure Nothing
      Just (Token VAR _ _) -> advance >> Just <$> varDeclaration
      _ -> Just <$> expressionStatement
  condition <-
    not <$> check SEMICOLON >>= \case
      True -> expression
      -- if there's no condition, it's an infinite loop
      False -> pure $ Expr.ELiteral $ LBool True
  consume SEMICOLON "Expect ';' after loop condition."
  increment <-
    not <$> check RIGHT_PAREN >>= \case
      True -> Just <$> expression
      False -> pure Nothing
  consume RIGHT_PAREN "Expect ')' after for clauses."
  whileStmtBody <-
    statement <&> \forStmtBody -> case increment of
      -- if there's an increment in the 'for', add it to the end of the 'while' body
      Just inc -> Stmt.SBlock [forStmtBody, Stmt.SExpression inc]
      Nothing -> forStmtBody
  -- while statement is built from the condition and the desugared for body & increment
  let whileStmt = Stmt.SWhile condition whileStmtBody
  -- finally, if there is an initializer (ex. 'for (var a = 1...)'), we prefix
  -- it to the desugared while loop
  let finishedForStmt = case initializer of
        Just ini -> Stmt.SBlock [ini, whileStmt]
        Nothing -> whileStmt
  pure finishedForStmt

whileStatement :: ParserM r Stmt.Stmt
whileStatement = do
  consume LEFT_PAREN "Expect '(' after 'while'."
  condition <- expression
  consume RIGHT_PAREN "Expect ')' after condition."
  Stmt.SWhile condition <$> statement

ifStatement :: ParserM r Stmt.Stmt
ifStatement = do
  consume LEFT_PAREN "Expect '(' after 'if'."
  condition <- expression
  consume RIGHT_PAREN "Expect ')' after if condition."
  thenBranch <- statement
  elseBranch <-
    match [ELSE] >>= \case
      True -> Just <$> statement
      False -> pure Nothing
  pure $ Stmt.SIf condition thenBranch elseBranch

block :: ParserM r [Stmt.Stmt]
block = (reverse <$> go []) <* consume RIGHT_BRACE "Expect '}' after block."
  where
    go accum = do
      cond <- liftA2 (&&) (not <$> check RIGHT_BRACE) (not <$> isAtEnd)
      if cond
        then do
          next <- declaration
          case next of
            Just n -> go (n : accum)
            Nothing -> go accum
        else pure accum

printStatement :: ParserM r Stmt.Stmt
printStatement = do
  value <- expression
  consume SEMICOLON "Expect ';' after value."
  pure $ Stmt.SPrint value

expressionStatement :: ParserM r Stmt.Stmt
expressionStatement = do
  expr <- expression
  consume SEMICOLON "Expect ';' after expression."
  pure $ Stmt.SExpression expr

expression :: ParserM r Expr
expression = assignment

assignment :: ParserM r Expr
assignment = do
  expr <- orP
  m <- match [EQUAL]
  if m
    then do
      equals <- previous
      value <- assignment
      case expr of
        Expr.EVariable name -> pure $ Expr.EAssign name value
        _ -> do
          -- the error is reported, but does not need to synchronize, hence why there's no `throwError`
          getPError equals "Invalid assignment target."
          pure expr
    else pure expr

orP :: ParserM r Expr
orP = andP >>= whileParseELogical andP [OR]

andP :: ParserM r Expr
andP = equality >>= whileParseELogical equality [AND]

equality :: ParserM r Expr
equality = comparison >>= whileParseEBinary comparison [BANG_EQUAL, EQUAL_EQUAL]

comparison :: ParserM r Expr
comparison = term >>= whileParseEBinary term [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

term :: ParserM r Expr
term = factor >>= whileParseEBinary factor [MINUS, PLUS]

factor :: ParserM r Expr
factor = unary >>= whileParseEBinary unary [SLASH, STAR]

unary :: ParserM r Expr
unary = do
  match [BANG, MINUS] >>= \m ->
    if m
      then liftA2 EUnary previous unary
      else
        primary

primary :: ParserM r Expr
primary = do
  t <- safePeek
  case t of
    Just (Token FALSE _ _) -> advance >> pure (ELiteral $ LBool False)
    Just (Token TRUE _ _) -> advance >> pure (ELiteral $ LBool True)
    Just (Token NIL _ _) -> advance >> pure (ELiteral LNil)
    Just (Token (NUMBER lit) _ _) -> advance >> pure (ELiteral $ LNumber lit)
    Just (Token (STRING lit) _ _) -> advance >> pure (ELiteral $ LString lit)
    Just tok@(Token IDENTIFIER _ _) -> advance >> pure (EVariable tok)
    Just (Token LEFT_PAREN _ _) ->
      advance >> do
        expr <- expression
        consume RIGHT_PAREN "Expect ')' after expression."
        pure $ EGrouping expr
    _ -> do
      p <- peek
      getPError p "Expect expression." >>= throwError

safePeek :: ParserM r (Maybe Token)
safePeek = do
  c <- not <$> isAtEnd
  if c then Just <$> peek else pure Nothing

{-# INLINE whileParse #-}
whileParse
  :: (Expr -> Token -> Expr -> Expr)
  -> ParserM r Expr
  -> [TokenType]
  -> Expr
  -> ParserM r Expr
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

whileParseEBinary :: ParserM r Expr -> [TokenType] -> Expr -> ParserM r Expr
whileParseEBinary = whileParse EBinary

whileParseELogical :: ParserM r Expr -> [TokenType] -> Expr -> ParserM r Expr
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
          if t.ttype == SEMICOLON
            then pure ()
            else case p.ttype of
              CLASS -> pure ()
              FUN -> pure ()
              VAR -> pure ()
              FOR -> pure ()
              IF -> pure ()
              WHILE -> pure ()
              PRINT -> pure ()
              RETURN -> pure ()
              _ -> advance >> go
        else do
          pure ()

consume :: TokenType -> ByteString -> ParserM r Token
consume ttype message =
  check ttype >>= \c ->
    if c
      then advance
      else
        peek >>= (`getPError` message) >>= throwError

getPError :: Token -> ByteString -> ParserM r ParseException
getPError token message = do
  Error.parseError token message
  pure ParseException

match :: [TokenType] -> ParserM r Bool
match = \case
  [] -> pure False
  (ttype : ttypes) ->
    check ttype >>= \c ->
      if c
        then advance >> pure True
        else match ttypes

check :: TokenType -> ParserM r Bool
check ttype =
  isAtEnd >>= \c ->
    if c
      then pure False
      else do
        p <- peek
        pure (p.ttype == ttype)

advance :: ParserM r Token
advance = do
  c <- not <$> isAtEnd
  when c $ modify' $ \pr -> pr {current = pr.current + 1}
  previous

isAtEnd :: ParserM r Bool
isAtEnd = (\t -> t.ttype == EOF) <$> peek

peek :: ParserM r Token
peek = get <&> \pr -> pr.tokens !! pr.current

previous :: ParserM r Token
previous = get <&> \pr -> pr.tokens !! (pr.current - 1)
