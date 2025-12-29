{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Optimizer (runOptimizer) where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Expr (Expr (..), LiteralValue (..))
import Expr qualified
import Stmt (Stmt (..))
import Stmt qualified
import TokenType (Token (..))

runOptimizer
  :: Vector Stmt.Stmt2 -> (Vector Stmt.Stmt2)
runOptimizer s = Vector.mapMaybe optimizeStatement s
  where
    optimizeStatement :: Stmt.Stmt2 -> Maybe Stmt.Stmt2
    optimizeStatement st = optimizeExpressions <$> eliminateRedundantStatements st

eliminateRedundantStatements :: Stmt.Stmt2 -> Maybe Stmt.Stmt2
eliminateRedundantStatements s = case s of
  SBlock a ->
    if
      | Vector.length a >= 1 ->
          let filtered = Vector.mapMaybe eliminateRedundantStatements a
           in case Vector.length filtered of
                0 -> Nothing
                1 -> Just $ filtered Vector.! 0
                _ -> Just $ SBlock filtered
      | otherwise -> Nothing
  -- SClass a b c -> s
  SExpression a -> case a of
    EVariable _ _ -> Nothing
    ELiteral _ -> Nothing
    _ -> Just s
  -- SFunction a -> s
  -- SIf a b c ->
  -- SPrint a -> s
  -- SReturn a b -> s
  -- SVar a b -> s
  SWhile a b -> (SWhile a) <$> eliminateRedundantStatements b
  _ -> Just s

optimizeExpressions :: Stmt.Stmt2 -> Stmt.Stmt2
optimizeExpressions s = case s of
  SBlock a -> SBlock $ fmap optimizeExpressions a
  SClass _a _b _c _ -> s
  SExpression a -> SExpression $ optimizeExpression a
  SFunction _a -> s
  SIf a b c ->
    SIf (optimizeExpression a) (optimizeExpressions b) (fmap optimizeExpressions c)
  SPrint a -> SPrint $ optimizeExpression a
  SReturn a b -> SReturn a $ fmap optimizeExpression b
  SVar a b -> SVar a $ fmap optimizeExpression b
  SWhile a b -> SWhile (optimizeExpression a) (optimizeExpressions b)

optimizeExpression :: Expr.Expr2 -> Expr.Expr2
optimizeExpression = computeConstantExpressions

computeConstantExpressions :: Expr.Expr2 -> Expr.Expr2
computeConstantExpressions e = case e of
  EAssign t a c -> EAssign t (computeConstantExpressions a) c
  EBinary l t r -> handleBin l t r
  ECall a t c -> ECall a t (fmap computeConstantExpressions c)
  EGet a t -> EGet a t
  ELiteral _ -> e
  ELogical l t r -> handleBin l t r
  ESet l t a -> ESet l t (computeConstantExpressions a)
  ESuper _ _ _ -> e
  EThis _ _ -> e
  EUnary t a -> EUnary t (computeConstantExpressions a)
  EVariable _ _ -> e
  where
    handleBin :: Expr.Expr2 -> Token -> Expr.Expr2 -> Expr.Expr2
    handleBin l t r =
      case ( computeConstantExpressions l
           , computeConstantExpressions r
           , Expr.isNumericalOperator t.ttype
           ) of
        (ELiteral (LNumber x), ELiteral (LNumber y), (Just op)) ->
          ELiteral $ x `op` y
        (l', r', _) -> EBinary l' t r'

-- SBlock a -> s
-- SClass a b c -> s
-- SExpression a ->s
-- SFunction a -> s
-- SIf a b c -> s
-- SPrint a -> s
-- SReturn a b -> s
-- SVar a b -> s
-- SWhile a b -> s
-- _ -> s
