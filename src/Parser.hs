{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Parser where

import Control.Monad (forM, when)
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Data.Functor ((<&>))
import Expr
import TokenType (Literal (..), Token (..), TokenType (..))

data Parser = Parser
  { current :: !Int
  , tokens :: ![Token]
  }

expression :: ParserM r Expr
expression = equality

type Error = String

type ParserM r a = RWS r [Error] Parser a

type ParserM2 r a = RWST r [Error] Parser (Either Error) a

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
    Just (Token FALSE _ _ _) -> pure $ ELiteral $ LitBool False
    Just (Token TRUE _ _ _) -> pure $ ELiteral $ LitBool True
    Just (Token NIL _ _ _) -> pure $ ELiteral LitNil
    Just (Token NUMBER _ lit _) -> pure $ ELiteral lit
    Just (Token STRING _ lit _) -> pure $ ELiteral lit
    Just (Token LEFT_PAREN _ _ _) -> do
      expr <- expression
      consume RIGHT_PAREN "Expect ')' after expression."
      pure $ EGrouping expr
    _ -> undefined
  where
    safePeek :: ParserM r (Maybe Token)
    safePeek = do
      c <- not <$> isAtEnd
      if c then Just <$> peek else pure Nothing

consume :: ParserM r Token
consume = do
  undefined

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
        pure $ p.ttype == ttype

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
