{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
  match [PRINT] >>= \case
    True -> printStatement
    False ->
      match [LEFT_BRACE] >>= \case
        True -> Stmt.SBlock <$> block
        False -> expressionStatement

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
  expr <- equality
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

equality :: ParserM r Expr
equality = comparison >>= whileParse comparison [BANG_EQUAL, EQUAL_EQUAL]

comparison :: ParserM r Expr
comparison = term >>= whileParse term [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

term :: ParserM r Expr
term = factor >>= whileParse factor [MINUS, PLUS]

factor :: ParserM r Expr
factor = unary >>= whileParse unary [SLASH, STAR]

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
  where
    safePeek :: ParserM r (Maybe Token)
    safePeek = do
      c <- not <$> isAtEnd
      if c then Just <$> peek else pure Nothing

{-# INLINE whileParse #-}
whileParse :: ParserM r Expr -> [TokenType] -> Expr -> ParserM r Expr
whileParse inner matchlist eAccum = do
  m <- match matchlist
  if m
    then do
      operator <- previous
      right <- inner
      whileParse inner matchlist $ EBinary eAccum operator right
    else
      pure eAccum

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
