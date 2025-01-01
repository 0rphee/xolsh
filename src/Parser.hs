{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move brackets to avoid $" #-}

module Parser (parse) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Data.ByteString.Char8 (ByteString)
import Data.Functor ((<&>))
import Error qualified
import Expr
import TokenType (Literal (..), Token (..), TokenType (..))

data Parser = Parser
  { current :: !Int
  , tokens :: ![Token]
  }
  deriving (Show)

expression :: ParserM r Expr
expression = equality

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

parse :: [Token] -> IO (Maybe Expr, Error.ErrorPresent)
parse tokens = do
  (r, w) <- (evalRWST . runExceptT) expression () initialParserState
  pure (either (const Nothing) Just r, w)
  where
    initialParserState = Parser {current = 0, tokens = tokens}

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
    Just (Token FALSE _ _ _) -> advance >> pure (ELiteral $ LitBool False)
    Just (Token TRUE _ _ _) -> advance >> pure (ELiteral $ LitBool True)
    Just (Token NIL _ _ _) -> advance >> (pure $ ELiteral LitNil)
    Just (Token NUMBER _ lit _) -> advance >> (pure $ ELiteral lit)
    Just (Token STRING _ lit _) -> advance >> (pure $ ELiteral lit)
    Just (Token LEFT_PAREN _ _ _) ->
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
synchronize = do
  advance
  go
  where
    go :: ParserM r ()
    go = do
      e <- not <$> isAtEnd
      if e
        then pure ()
        else do
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
