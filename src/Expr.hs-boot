{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module Expr (IPhase (..), LiteralValue, Expr) where

data LiteralValue

data IPhase = PH1 | PH2

type role Expr nominal

data Expr (phase :: IPhase)
