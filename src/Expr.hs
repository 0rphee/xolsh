{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Expr
  ( IPhase (..),
    Expr (..),
    LiteralValue (..),
    Expr1,
    Expr2,
    Callable (..),
    eqLiteralValue,
    XEnvDistance,
    XAccessInfo,
    XToken,
    XEGrouping,
    AccessInfo (..),
    LoxRuntimeFunction (..),
    LoxNativeFunction (..),
    LoxRuntimeClass (..),
    isNumericalOperator,
    literalValueType,
  )
where

import Bluefin.EarlyReturn (EarlyReturn)
import Bluefin.Eff
import Bluefin.Exception (Exception)
import Bluefin.IO (IOE)
import Bluefin.State (State)
import Data.ByteString.Short (ShortByteString)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Data.Void (Void)
import Environment (Environment, InterpreterState)
import Error qualified
import Stmt qualified
import TokenType (Token (..), TokenType (..))

data IPhase = PH1 | PH2

type Expr1 = Expr PH1

type Expr2 = Expr PH2

data AccessInfo = MkAccessInfo {distance :: {-# UNPACK #-} !Int, index :: {-# UNPACK #-} !Int}
  deriving (Show, Eq)

type family XEnvDistance (phase :: IPhase) where
  -- later we want to change '()' for Token
  XEnvDistance PH1 = ()
  XEnvDistance PH2 = AccessInfo

type family XAccessInfo (phase :: IPhase) where
  XAccessInfo PH1 = Token
  XAccessInfo PH2 = AccessInfo

type family XToken (phase :: IPhase) where
  XToken PH1 = Token
  XToken PH2 = ()

type family XCallToken (phase :: IPhase) where
  XCallToken PH1 = Token
  XCallToken PH2 = Int

type family XEGrouping (phase :: IPhase) where
  XEGrouping PH1 = Expr PH1
  XEGrouping PH2 = Void

type family XIsTailCall (phase :: IPhase) where
  XIsTailCall PH1 = ()
  XIsTailCall PH2 = Bool

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
    ECall
      !(Expr phase)
      !(XCallToken phase)
      !(Vector (Expr phase))
      (XIsTailCall phase)
  | -- | > EGet
    -- >   Expr phase -- object
    -- >   Token -- name
    EGet !(Expr phase) !Token
  | -- | > EGrouping
    -- >   (Expr phase)-- expression
    EGrouping !(XEGrouping phase)
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
  | CNativeFunction !LoxNativeFunction
  | CClass !(IORef LoxRuntimeClass)
  deriving (Eq)

data LoxRuntimeFunction = LRFunction
  { fun_toString :: !ShortByteString,
    fun_params :: !(Vector AccessInfo),
    fun_token_line :: !Int,
    fun_closure :: !Environment, -- closure environment
    fun_isInitializer :: !Bool,
    fun_body :: !(Vector Stmt.Stmt2)
  }

instance Eq LoxRuntimeFunction where
  a == b =
    (a.fun_isInitializer == b.fun_isInitializer)
      && (a.fun_params == b.fun_params)
      && (a.fun_toString == a.fun_toString)
      && (a.fun_closure == b.fun_closure)

data LoxNativeFunction = LNFunction
  { ln_fun_arity :: !Int,
    ln_fun_call ::
      forall es io ex st.
      (io :> es, ex :> es, st :> es) =>
      IOE io ->
      Exception Error.RuntimeException ex ->
      State InterpreterState st ->
      Vector LiteralValue -> -- arguments
      Eff es LiteralValue
  }

instance Eq LoxNativeFunction where
  a == b =
    (a.ln_fun_arity == b.ln_fun_arity)

data LoxRuntimeClass = LRClass
  { class_name :: !ShortByteString,
    class_arity :: !Int,
    class_methods :: !(IORef (Map ShortByteString LoxRuntimeFunction)),
    class_superclass :: !(Maybe (IORef LoxRuntimeClass)),
    class_call ::
      forall es io ex st ret.
      (io :> es, ex :> es, st :> es, ret :> es) =>
      IOE io ->
      Exception Error.RuntimeException ex ->
      State InterpreterState st ->
      EarlyReturn LiteralValue ret ->
      IORef LoxRuntimeClass -> -- reference to itself
      Vector LiteralValue -> -- arguments
      Eff es LiteralValue
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
      { _LInstanceFields :: !(IORef (Map ShortByteString LiteralValue)),
        _LInstanceClass :: !(IORef LoxRuntimeClass)
      }

isNumericalOperator :: TokenType -> Maybe (Double -> Double -> LiteralValue)
isNumericalOperator = \case
  MINUS -> Just $ \x y -> LNumber (x - y)
  PLUS -> Just $ \x y -> LNumber (x + y)
  SLASH -> Just $ \x y -> LNumber (x / y)
  STAR -> Just $ \x y -> LNumber (x * y)
  GREATER -> Just $ \x y -> LBool (x > y)
  GREATER_EQUAL -> Just $ \x y -> LBool (x >= y)
  LESS -> Just $ \x y -> LBool (x < y)
  LESS_EQUAL -> Just $ \x y -> LBool (x <= y)
  _ -> Nothing

literalValueType :: LiteralValue -> String
literalValueType = \case
  LNil -> "LNil"
  LBool v -> "LBool: " <> show v
  LString v -> "LString: " <> show v
  LNumber n -> "LNumber: " <> show n
  LCallable _ -> "LCallable"
  LInstance _ _ -> "LInstance"

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
