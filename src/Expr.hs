module Expr (Expr (..)) where

import TokenType (Literal (..), Token (..))

data Expr
  = -- | > EBinary
    -- >   Expr -- left
    -- >   Token -- operator
    -- >   Expr -- right
    EBinary
      !Expr
      !Token
      !Expr
  | -- | > EGrouping
    -- >   Expr -- expression
    EGrouping
      !Expr
  | -- | > ELiteral
    -- >   Literal -- value
    ELiteral !Literal
  | -- | > EUnary
    -- >   Token -- operator
    -- >   Expr -- expression
    EUnary !Token !Expr
