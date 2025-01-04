module Expr (Expr (..), LiteralValue (..)) where

{-Literal (..),-}
import Data.ByteString.Char8 (ByteString)
import TokenType (Token (..))

data Expr
  = -- | > EAssign
    -- >   Token -- name
    -- >   Expr  -- value
    EAssign
      !Token
      !Expr
  | -- | > EBinary
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
    -- >   LiteralValue -- value
    ELiteral !LiteralValue
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
  deriving (Eq)
