{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Stmt (Stmt1, Stmt2, Stmt (..), FunctionH (..), FunctionH1, FunctionH2) where

import Data.Vector (Vector)
import {-# SOURCE #-} Expr qualified
import TokenType qualified

type Stmt1 = Stmt Expr.PH1

type Stmt2 = Stmt Expr.PH2

type family XNameInfo (phase :: Expr.IPhase) where
  XNameInfo Expr.PH1 = TokenType.Token
  XNameInfo Expr.PH2 = Expr.NameInfo

type family XNameInfoFunction (phase :: Expr.IPhase) where
  XNameInfoFunction Expr.PH1 = ()
  XNameInfoFunction Expr.PH2 = Expr.NameInfo

data Stmt (phase :: Expr.IPhase)
  = -- | > SBlock
    -- >   [Stmt phase] -- statements
    -- Example:
    -- > {
    -- >   print "One statement.";
    -- >   print "Two statements.";
    -- > }
    SBlock !(Vector (Stmt phase))
  | -- | > SClass
    -- >   Token -- name
    -- >   Maybe (Token, XEnvDistance phase) -- superclass
    -- >   Vector (FunctionH phase) -- methods
    SClass
      !(XNameInfo phase)
      !(Maybe (TokenType.Token, Expr.XEnvDistance phase))
      !(Vector (FunctionH phase))
  | -- | > SExpression
    -- >   Expr phase -- expression
    -- Example:
    -- > expr;
    SExpression !(Expr.Expr phase)
  | -- | > SFunction
    -- >   FunctionH phase
    -- Example:
    -- > fun printSum(a, b) {
    -- >   print a + b;
    -- > }
    SFunction !(FunctionH phase)
  | -- | > SIf
    -- >   Expr phase -- condition
    -- >   (Stmt phase) -- thenBranch
    -- >   Maybe (Stmt phase) -- elseBranch
    -- Example:
    -- > if (condition) {
    -- >   stmt;
    -- > } else {
    -- >   stmt;
    -- > }
    SIf !(Expr.Expr phase) !(Stmt phase) !(Maybe (Stmt phase))
  | -- | > SPrint
    -- >   Expr phase -- expression
    -- Example:
    -- > print expr;
    SPrint !(Expr.Expr phase)
  | -- | > SReturn
    -- >   TokenType.Token   -- keyword
    -- >   (Maybe Expr.Expr phase) -- value
    SReturn !TokenType.Token !(Maybe (Expr.Expr phase))
  | -- | > SVar
    -- >   Token -- name
    -- >   Maybe (Expr phase) -- initializer
    -- Example:
    -- > var name = expr;
    -- > var name;
    SVar !(XNameInfo phase) !(Maybe (Expr.Expr phase))
  | -- | > SWhile
    -- >   Expr phase -- condition
    -- >   (Stmt phase) -- body
    -- Example:
    -- > while (condition) {
    -- >   stmts;
    -- > }
    SWhile !(Expr.Expr phase) (Stmt phase)

type FunctionH1 = FunctionH Expr.PH1

type FunctionH2 = FunctionH Expr.PH2

data FunctionH (phase :: Expr.IPhase)
  = -- | > FFunctionH
    -- >   Token          -- name
    -- >   (Vector Token) -- params
    -- >   (Vector (Stmt phase))  -- body
    FFunctionH
      !TokenType.Token
      !(XNameInfoFunction phase)
      !(Vector TokenType.Token)
      !(Vector (Stmt phase))
