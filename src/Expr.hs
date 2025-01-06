module Expr (Expr (..), LiteralValue (..)) where

import Data.ByteString.Char8 (ByteString)
import Data.Vector (Vector)
import Environment (InterpreterM)
import TokenType (Token (..))

data Expr
  = -- | > EAssign
    -- >   Token -- name
    -- >   Expr  -- value
    EAssign !Token !Expr
  | -- | > EBinary
    -- >   Expr -- left
    -- >   Token -- operator
    -- >   Expr -- right
    EBinary !Expr !Token !Expr
  | -- | > ECall
    -- >   Expr   -- callee
    -- >   Token  -- paren
    -- >   [Expr] -- arguments
    ECall !Expr !Token !(Vector Expr)
  | -- | > EGrouping
    -- >   Expr -- expression
    EGrouping
      !Expr
  | -- | > ELiteral
    -- >   LiteralValue -- value
    ELiteral !LiteralValue
  | -- | > ELogical
    -- >   Expr -- left
    -- >   Token -- operator
    -- >   Expr -- right
    ELogical !Expr !Token !Expr
  | -- | > EUnary
    -- >   Token -- operator
    -- >   Expr -- expression
    EUnary !Token !Expr
  | -- | > EVariable
    -- >   Token -- name
    EVariable !Token

data LiteralValue
  = LNil
  | LBool !Bool
  | LString !ByteString
  | LNumber !Double
  | LCallable
      { callable_arity :: Int
      , callable_call
          :: Vector Expr.LiteralValue
          -> InterpreterM LiteralValue
      , callable_toString :: ByteString
      }

instance Eq LiteralValue where
  a == b = case (a, b) of
    (LNil, LNil) -> True
    (LBool x, LBool y) -> x == y
    (LString x, LString y) -> x == y
    -- Lox considers NaN equal to NaN, contrary to what (==) does (7.2.5)
    (Expr.LNumber vl, Expr.LNumber vr) | isNaN vl && isNaN vr -> True
    _ -> False
