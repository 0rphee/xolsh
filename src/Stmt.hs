module Stmt where

import Expr qualified
import TokenType qualified

data Stmt
  = -- | > SBlock
    -- >   [Stmt] -- statements
    SBlock
      [Stmt]
  | -- | > SExpression
    -- >   Expr -- expression
    SExpression !Expr.Expr
  | -- | > SPrint
    -- >   Expr -- expression
    SPrint !Expr.Expr
  | -- | > SVar
    -- >   Token -- name
    -- >   Maybe Expr  -- initializer
    SVar !TokenType.Token !(Maybe Expr.Expr)
