{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Expr
  ( IPhase (..)
  , LiteralValue
  , Expr
  , XEnvDistance
  , Callable
  , NameInfo (..)
  )
where

data LiteralValue

data IPhase = PH1 | PH2

type role Expr nominal

data Expr (phase :: IPhase)

data NameInfo = NameInfo
  {nameInfo_scope :: {-# UNPACK #-} !Int, nameInfo_index :: {-# UNPACK #-} !Int}

instance Eq NameInfo

instance Show NameInfo

type family XEnvDistance (phase :: IPhase) where
  XEnvDistance PH1 = ()
  XEnvDistance PH2 = NameInfo

data Callable
