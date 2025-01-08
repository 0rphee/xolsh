{-# LANGUAGE DataKinds #-}

module Stmt (Stmt1, Stmt2, Stmt (..), FunctionH (..), FunctionH1, FunctionH2) where

import Data.Vector (Vector)
import {-# SOURCE #-} Expr qualified
import TokenType qualified

type Stmt1 = Stmt Expr.PH1

type Stmt2 = Stmt Expr.PH2

data Stmt (phase :: Expr.IPhase)
  = -- | > SBlock
    -- >   [Stmt phase] -- statements
    SBlock !(Vector (Stmt phase))
  | -- | > SClass
    -- >   Token -- name
    -- >   Vector (FunctionH phase) -- methods
    SClass !TokenType.Token !(Vector (FunctionH phase))
  | -- | > SExpression
    -- >   Expr phase -- expression
    SExpression !(Expr.Expr phase)
  | -- | > SFunction
    -- >   FunctionH phase
    SFunction !(FunctionH phase)
  | -- | > SIf
    -- >   Expr phase -- condition
    -- >   (Stmt phase) -- thenBranch
    -- >   Maybe (Stmt phase) -- elseBranch
    SIf !(Expr.Expr phase) !(Stmt phase) !(Maybe (Stmt phase))
  | -- | > SPrint
    -- >   Expr phase -- expression
    SPrint !(Expr.Expr phase)
  | -- | > SReturn
    -- >   TokenType.Token   -- keyword
    -- >   (Maybe Expr.Expr phase) -- value
    SReturn !TokenType.Token !(Maybe (Expr.Expr phase))
  | -- | > SVar
    -- >   Token -- name
    -- >   Maybe (Expr phase) -- initializer
    SVar !TokenType.Token !(Maybe (Expr.Expr phase))
  | -- | > SWhile
    -- >   Expr phase -- condition
    -- >   (Stmt phase) -- body
    SWhile !(Expr.Expr phase) (Stmt phase)

type FunctionH1 = FunctionH Expr.PH1

type FunctionH2 = FunctionH Expr.PH2

data FunctionH (phase :: Expr.IPhase)
  = -- | > FFunctionH
    -- >   Token          -- name
    -- >   (Vector Token) -- params
    -- >   (Vector (Stmt phase))  -- body
    FFunctionH !TokenType.Token !(Vector TokenType.Token) !(Vector (Stmt phase))
