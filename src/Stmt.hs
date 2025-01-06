module Stmt where

import Data.Vector (Vector)
import {-# SOURCE #-} Expr qualified
import TokenType qualified

data Stmt
  = -- | > SBlock
    -- >   [Stmt] -- statements
    SBlock !(Vector Stmt)
  | -- | > SExpression
    -- >   Expr -- expression
    SExpression !Expr.Expr
  | -- | > SFunction
    -- >   Token          -- name
    -- >   (Vector Token) -- params
    -- >   (Vector Stmt)  -- body
    SFunction !TokenType.Token !(Vector TokenType.Token) !(Vector Stmt)
  | -- | > SIf
    -- >   Expr -- condition
    -- >   Stmt -- thenBranch
    -- >   Maybe Stmt -- elseBranch
    SIf !Expr.Expr !Stmt !(Maybe Stmt)
  | -- | > SPrint
    -- >   Expr -- expression
    SPrint !Expr.Expr
  | -- | > SReturn
    -- >   TokenType.Token   -- keyword
    -- >   (Maybe Expr.Expr) -- value
    SReturn !TokenType.Token !(Maybe Expr.Expr)
  | -- | > SVar
    -- >   Token -- name
    -- >   Maybe Expr  -- initializer
    SVar !TokenType.Token !(Maybe Expr.Expr)
  | -- | > SWhile
    -- >   Expr -- condition
    -- >   Stmt -- body
    SWhile !Expr.Expr Stmt
