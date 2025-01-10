{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Expr (IPhase (..), LiteralValue, Expr, XEnvDistance, Callable) where

data LiteralValue

data IPhase = PH1 | PH2

type role Expr nominal

data Expr (phase :: IPhase)

type family XEnvDistance (phase :: IPhase) where
  XEnvDistance PH1 = ()
  XEnvDistance PH2 = Int

data Callable
