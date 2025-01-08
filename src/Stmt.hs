{-# LANGUAGE DataKinds #-}

module Stmt where

import Data.Vector (Vector)
import {-# SOURCE #-} Expr qualified
import TokenType qualified

type Stmt1 = Stmt Expr.PH1

type Stmt2 = Stmt Expr.PH2

data Stmt (phase :: Expr.IPhase)
  = -- | > SBlock
    -- >   [Stmt] -- statements
    SBlock !(Vector (Stmt phase))
  | -- | > SExpression
    -- >   Expr -- expression
    SExpression !(Expr.Expr phase)
  | -- | > SFunction
    -- >   Token          -- name
    -- >   (Vector Token) -- params
    -- >   (Vector (Stmt phase))  -- body
    SFunction !TokenType.Token !(Vector TokenType.Token) !(Vector (Stmt phase))
  | -- | > SIf
    -- >   Expr -- condition
    -- >   (Stmt phase) -- thenBranch
    -- >   Maybe (Stmt phase) -- elseBranch
    SIf !(Expr.Expr phase) !(Stmt phase) !(Maybe (Stmt phase))
  | -- | > SPrint
    -- >   Expr -- expression
    SPrint !(Expr.Expr phase)
  | -- | > SReturn
    -- >   TokenType.Token   -- keyword
    -- >   (Maybe Expr.Expr phase) -- value
    SReturn !TokenType.Token !(Maybe (Expr.Expr phase))
  | -- | > SVar
    -- >   Token -- name
    -- >   Maybe Expr  -- initializer
    SVar !TokenType.Token !(Maybe (Expr.Expr phase))
  | -- | > SWhile
    -- >   Expr -- condition
    -- >   (Stmt phase) -- body
    SWhile !(Expr.Expr phase) (Stmt phase)
