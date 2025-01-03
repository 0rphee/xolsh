module Expr (Expr (..), LiteralValue (..)) where

{-Literal (..),-}
import Data.ByteString.Char8 (ByteString)
import TokenType (Token (..))

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
    -- >   LiteralValue -- value
    ELiteral !LiteralValue
  | -- | > EUnary
    -- >   Token -- operator
    -- >   Expr -- expression
    EUnary !Token !Expr

data LiteralValue
  = LNil
  | LBool !Bool
  | LString !ByteString
  | LNumber !Double
  deriving (Eq)
