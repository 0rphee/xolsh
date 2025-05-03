{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Expr
  ( IPhase (..)
  , Expr (..)
  , LiteralValue (..)
  , Expr1
  , Expr2
  , Callable (..)
  , eqLiteralValue
  , XEnvDistance
  , LoxRuntimeFunction (..)
  , LoxRuntimeClass (..)
  , NameInfo (..)
  )
where

import Data.ByteString.Short (ShortByteString)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Environment (Environment, InterpreterM)
import Stmt qualified
import TokenType (Token (..))

data IPhase = PH1 | PH2

type Expr1 = Expr PH1

type Expr2 = Expr PH2

data NameInfo = NameInfo
  {nameInfo_scope :: {-# UNPACK #-} !Int, nameInfo_index :: {-# UNPACK #-} !Int}
  deriving (Eq, Show)

type family XEnvDistance (phase :: IPhase) where
  XEnvDistance PH1 = ()
  XEnvDistance PH2 = NameInfo

data Expr (phase :: IPhase)
  = -- | > EAssign
    -- >   Token -- name
    -- >   Expr  -- value
    EAssign !Token !(Expr phase) !(XEnvDistance phase)
  | -- | > EBinary
    -- >   (Expr phase)-- left
    -- >   Token -- operator
    -- >   (Expr phase)-- right
    EBinary !(Expr phase) !Token !(Expr phase)
  | -- | > ECall
    -- >   (Expr phase)  -- callee
    -- >   Token  -- paren
    -- >   [(Expr phase)] -- arguments
    ECall !(Expr phase) !Token !(Vector (Expr phase))
  | -- | > EGet
    -- >   Expr phase -- object
    -- >   Token -- name
    EGet !(Expr phase) !Token
  | -- | > EGrouping
    -- >   (Expr phase)-- expression
    EGrouping
      !(Expr phase)
  | -- | > ELiteral
    -- >   LiteralValue -- value
    ELiteral !LiteralValue
  | -- | > ELogical
    -- >   (Expr phase)-- left
    -- >   Token -- operator
    -- >   (Expr phase)-- right
    ELogical !(Expr phase) !Token !(Expr phase)
  | -- | > ESet
    -- >   Expr phase -- object
    -- >   Token -- name
    -- >   Expr phase -- value
    ESet !(Expr phase) !Token !(Expr phase)
  | -- | > ESuper
    -- >   Token -- keyword
    -- >   !(XEnvDistance phase)
    -- >   Token -- method
    ESuper !Token !(XEnvDistance phase) !Token
  | -- | > EThis
    -- >   Token -- keyword
    -- >   XenvDistance phase -- distance
    EThis !Token !(XEnvDistance phase)
  | -- | > EUnary
    -- >   Token -- operator
    -- >   (Expr phase)-- expression
    EUnary !Token !(Expr phase)
  | -- | > EVariable
    -- >   Token -- name
    EVariable !Token !(XEnvDistance phase)

data Callable
  = CFunction !LoxRuntimeFunction
  | CClass !(IORef LoxRuntimeClass)
  deriving (Eq)

data LoxRuntimeFunction = LRFunction
  { fun_toString :: !ShortByteString
  , fun_arity :: !Int
  , fun_closure :: !Environment -- closure environment
  , fun_isInitializer :: !Bool
  , fun_call
      :: ( Token -- function token
           -> Vector Stmt.Stmt2 -- body of lox function
           -> Vector Token -- parameters
           -> Vector LiteralValue -- arguments
           -> Bool -- isInitalizer
           -> InterpreterM LiteralValue -- this function is ignored when calling native functions
         )
      -> Vector LiteralValue -- arguments
      -> InterpreterM LiteralValue
  }

instance Eq LoxRuntimeFunction where
  a == b =
    (a.fun_arity == b.fun_arity)
      && (a.fun_isInitializer == b.fun_isInitializer)
      && (a.fun_toString == a.fun_toString)
      && (a.fun_closure == b.fun_closure)

data LoxRuntimeClass = LRClass
  { class_name :: !ShortByteString
  , class_arity :: !Int
  , class_methods :: !(IORef (Map ShortByteString LoxRuntimeFunction))
  , class_superclass :: !(Maybe (IORef LoxRuntimeClass))
  , class_call
      :: IORef LoxRuntimeClass -- reference to itself
      -> Vector LiteralValue -- arguments
      -> InterpreterM LiteralValue
  }

instance Eq LoxRuntimeClass where
  a == b =
    (a.class_arity == b.class_arity)
      && (a.class_methods == b.class_methods)
      && (a.class_name == a.class_name)
      && (a.class_superclass == b.class_superclass)

data LiteralValue
  = LNil
  | LBool !Bool
  | LString !ShortByteString
  | LNumber !Double
  | LCallable !Callable
  | LInstance
      { _LInstanceFields :: !(IORef (Map ShortByteString LiteralValue))
      , _LInstanceClass :: !(IORef LoxRuntimeClass)
      }

instance Eq LiteralValue where
  (==) = eqLiteralValue

eqLiteralValue :: LiteralValue -> LiteralValue -> Bool
eqLiteralValue a b = case (a, b) of
  (LNil, LNil) -> True
  (LBool x, LBool y) -> x == y
  (LString x, LString y) -> x == y
  -- Lox considers NaN equal to NaN, contrary to what (==) does (7.2.5)
  (LNumber x, LNumber y) -> if isNaN x then isNaN y else x == y
  (LCallable x, LCallable y) -> x == y
  (LInstance fields1 mthds1, LInstance fields2 mthds2) ->
    (fields1 == fields2)
      && (mthds1 == mthds2)
  _ -> False
