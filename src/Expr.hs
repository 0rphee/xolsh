module Expr where

import Data.ByteString.Char8 as B
import Data.Vector as V

-- Expression
data Expr = Expr EqualityExpr
  deriving (Show)

-- Equality
data EqualityExpr
  = EqualityExpr ComparisonExpr (Vector (EqualityOP, ComparisonExpr))
  deriving (Show)

data EqualityOP
  = ENOTEQ
  | EEQ
  deriving (Show)

-- Comparison
data ComparisonExpr
  = ComparisonExpr TermExpr (Vector (ComparisonOP, TermExpr))
  deriving (Show)

data ComparisonOP
  = CGT -- >
  | CGTEQ -- >=
  | CLT -- <
  | CLTEQ -- <=
  deriving (Show)

-- Term
data TermExpr
  = TermExpr FactorExpr (Vector (TermOP, FactorExpr))
  deriving (Show)

data TermOP
  = TMinus
  | TPlus
  deriving (Show)

-- Factor
data FactorExpr
  = FactorExpr UnaryExpr (Vector (FactorOP, UnaryExpr))
  deriving (Show)

data FactorOP
  = FDivision
  | FMultiplication
  deriving (Show)

-- Unary
data UnaryExpr
  = UnaryExpr UnaryOP UnaryExpr
  | PrimaryExpr
  deriving (Show)

data UnaryOP
  = UNegate
  | UMinus
  deriving (Show)

-- Primary
data PrimaryExpr
  = PNumberExpr Double
  | PStrExpr ByteString
  | PBoolConstExpr Bool
  | PNilExpr
  | PGroupedExpr Expr
  deriving (Show)
