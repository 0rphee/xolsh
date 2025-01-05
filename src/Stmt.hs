module Stmt where

import Expr qualified
import TokenType qualified

data Stmt
  = -- | > SBlock
    -- >   [Stmt] -- statements
    SBlock ![Stmt]
  | -- | > SExpression
    -- >   Expr -- expression
    SExpression !Expr.Expr
  | -- | > SIf
    -- >   Expr -- condition
    -- >   Stmt -- thenBranch
    -- >   Maybe Stmt -- elseBranch
    SIf !Expr.Expr !Stmt (Maybe Stmt)
  | -- | > SPrint
    -- >   Expr -- expression
    SPrint !Expr.Expr
  | -- | > SVar
    -- >   Token -- name
    -- >   Maybe Expr  -- initializer
    SVar !TokenType.Token !(Maybe Expr.Expr)
  | -- | > SWhile
    -- >   Expr -- condition
    -- >   Stmt -- body
    SWhile !Expr.Expr Stmt
  deriving (Show)
