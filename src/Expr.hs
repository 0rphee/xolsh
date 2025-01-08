{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Expr (IPhase (..), Expr (..), LiteralValue (..), Expr1, Expr2) where

import Data.ByteString.Char8 (ByteString)
import Data.Vector (Vector)
import Environment (Environment, InterpreterM)
import Stmt qualified
import TokenType (Token (..))

data IPhase = PH1 | PH2

type Expr1 = Expr PH1

type Expr2 = Expr PH2

type family XEnvDistance (phase :: IPhase) where
  XEnvDistance PH1 = ()
  XEnvDistance PH2 = Int

data Expr (phase :: IPhase)
  = -- | > EAssign
    -- >   Token -- name
    -- >   Expr  -- value
    EAssign !Token !(Expr phase) !(XEnvDistance phase)
  | -- | > EBinary
    -- >   (Expr phase)-- left
    -- >   Token -- operator
    -- >   (Expr phase)-- right
    EBinary !(Expr phase) !Token !(Expr phase)
  | -- | > ECall
    -- >   (Expr phase)  -- callee
    -- >   Token  -- paren
    -- >   [(Expr phase)] -- arguments
    ECall !(Expr phase) !Token !(Vector (Expr phase))
  | -- | > EGrouping
    -- >   (Expr phase)-- expression
    EGrouping
      !(Expr phase)
  | -- | > ELiteral
    -- >   LiteralValue -- value
    ELiteral !LiteralValue
  | -- | > ELogical
    -- >   (Expr phase)-- left
    -- >   Token -- operator
    -- >   (Expr phase)-- right
    ELogical !(Expr phase) !Token !(Expr phase)
  | -- | > EUnary
    -- >   Token -- operator
    -- >   (Expr phase)-- expression
    EUnary !Token !(Expr phase)
  | -- | > EVariable
    -- >   Token -- name
    EVariable !Token !(XEnvDistance phase)

data LiteralValue
  = LNil
  | LBool !Bool
  | LString !ByteString
  | LNumber !Double
  | LCallable
      { callable_toString :: !ByteString
      , callable_params :: !(Vector Token)
      , callable_call
          :: ( Environment -- closure environment
               -> Vector Stmt.Stmt2 -- body of lox function
               -> Vector LiteralValue -- arguments
               -> InterpreterM LiteralValue -- this function is ignored when calling native functions
             )
          -> Vector LiteralValue -- arguments
          -> InterpreterM LiteralValue
      }

instance Eq LiteralValue where
  a == b = case (a, b) of
    (LNil, LNil) -> True
    (LBool x, LBool y) -> x == y
    (LString x, LString y) -> x == y
    -- Lox considers NaN equal to NaN, contrary to what (==) does (7.2.5)
    (Expr.LNumber x, Expr.LNumber y) -> if isNaN x then isNaN y else x == y
    _ -> False
