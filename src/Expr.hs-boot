{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Expr
  ( IPhase (..)
  , LiteralValue
  , Expr
  , XEnvDistance
  , Callable
  , AccessInfo (..)
  , XAccessInfo
  , literalValueType
  )
where

import TokenType qualified

data LiteralValue

data IPhase = PH1 | PH2

type role Expr nominal

data Expr (phase :: IPhase)

data AccessInfo = MkAccessInfo {distance :: !Int, index :: !Int}

instance Show AccessInfo

instance Eq AccessInfo

type family XEnvDistance (phase :: IPhase) where
  XEnvDistance PH1 = ()
  XEnvDistance PH2 = AccessInfo

type family XAccessInfo (phase :: IPhase) where
  XAccessInfo PH1 = TokenType.Token
  XAccessInfo PH2 = AccessInfo

data Callable

literalValueType :: LiteralValue -> String
