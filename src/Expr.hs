{-# LANGUAGE DeriveAnyClass #-}

module Expr where

import Control.DeepSeq
import Data.ByteString.Char8 as B
import Data.Vector as V
import GHC.Generics

-- Expression
data Expr = Expr EqualityExpr
  deriving (Show, Generic, NFData)

-- Equality
data EqualityExpr
  = EqualityExpr ComparisonExpr (Vector (EqualityOP, ComparisonExpr))
  deriving (Show, Generic, NFData)

data EqualityOP
  = ENOTEQ
  | EEQ
  deriving (Show, Generic, NFData)

-- Comparison
data ComparisonExpr
  = ComparisonExpr TermExpr (Vector (ComparisonOP, TermExpr))
  deriving (Show, Generic, NFData)

data ComparisonOP
  = CGT -- >
  | CGTEQ -- >=
  | CLT -- <
  | CLTEQ -- <=
  deriving (Show, Generic, NFData)

-- Term
data TermExpr
  = TermExpr FactorExpr (Vector (TermOP, FactorExpr))
  deriving (Show, Generic, NFData)

data TermOP
  = TMinus
  | TPlus
  deriving (Show, Generic, NFData)

-- Factor
data FactorExpr
  = FactorExpr UnaryExpr (Vector (FactorOP, UnaryExpr))
  deriving (Show, Generic, NFData)

data FactorOP
  = FDivision
  | FMultiplication
  deriving (Show, Generic, NFData)

-- Unary
data UnaryExpr
  = UnaryExpr UnaryOP UnaryExpr
  | UPrimaryExpr PrimaryExpr
  deriving (Show, Generic, NFData)

data UnaryOP
  = UNegate
  | UMinus
  deriving (Show, Generic, NFData)

-- Primary
data PrimaryExpr
  = PNumberExpr Double
  | PStrExpr ByteString
  | PBoolConstExpr Bool
  | PNilExpr
  | PGroupedExpr Expr
  deriving (Show, Generic, NFData)
