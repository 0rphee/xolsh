{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Optimizer (runOptimizer) where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Expr (Expr (..))
import Stmt (Stmt (..))
import Stmt qualified

runOptimizer
  :: Vector Stmt.Stmt2 -> (Vector Stmt.Stmt2)
runOptimizer s = Vector.mapMaybe optimizeStatement s

optimizeStatement :: Stmt.Stmt2 -> Maybe Stmt.Stmt2
optimizeStatement s = case s of
  --  !(Vector (Stmt phase))
  SBlock a ->
    if
      | Vector.length a >= 1 -> Just $ SBlock $ Vector.mapMaybe optimizeStatement a
      | otherwise -> Nothing
  --  !TokenType.Token !(Maybe (TokenType.Token, Expr.XEnvDistance phase)) !(Vector (FunctionH phase))
  -- SClass a b c -> s
  --  !(Expr.Expr phase)
  SExpression a -> case a of
    EVariable _ _ -> Nothing
    _ -> Just s
  --  !(FunctionH phase)
  -- SFunction a -> s
  --  !(Expr.Expr phase) !(Stmt phase) !(Maybe (Stmt phase))
  -- SIf a b c ->
  --  !(Expr.Expr phase)
  -- SPrint a -> s
  --  !TokenType.Token !(Maybe (Expr.Expr phase))
  -- SReturn a b -> s
  --  !TokenType.Token !(Maybe (Expr.Expr phase))
  -- SVar a b -> s
  --  !(Expr.Expr phase) (Stmt phase)
  SWhile a b -> (SWhile a) <$> optimizeStatement b
  _ -> Just s
