{-# LANGUAGE DataKinds #-}
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
  )
where

import Data.ByteString.Char8 (ByteString)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Environment (ClassMethodChain, Environment, InterpreterM)
import Stmt qualified
import TokenType (Token (..))

data IPhase = PH1 | PH2

type Expr1 = Expr PH1

type Expr2 = Expr PH2

type family XEnvDistance (phase :: IPhase) where
  XEnvDistance PH1 = ()
  XEnvDistance PH2 = Int

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
  = CFunction
      { callable_toString :: !ByteString
      , callable_arity :: !Int
      , callable_closure :: !Environment -- closure environment
      , callable_isInitializer :: !Bool
      , callable_call
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
  | CClass
      { callable_toString :: !ByteString
      , callable_arity :: !Int
      , callable_call
          :: ( Token -- function token
               -> Vector Stmt.Stmt2 -- body of lox function
               -> Vector Token -- parameters
               -> Vector LiteralValue -- arguments
               -> Bool -- isInitalizer
               -> InterpreterM LiteralValue -- this function is ignored when calling native functions
             )
          -> Vector LiteralValue -- arguments
          -> InterpreterM LiteralValue
      , class_methods :: !ClassMethodChain
      }

instance Eq Callable where
  (==) = eqCallable

eqCallable :: Callable -> Callable -> Bool
eqCallable x y =
  case (x, y) of
    ( CFunction name1 arity1 closure1 isinit1 _
      , CFunction name2 arity2 closure2 isinit2 _
      ) ->
        (name1 == name2)
          && (arity1 == arity2)
          && (isinit1 == isinit2)
          && closure1 == closure2
    (CClass name1 arity1 _ methds1, CClass name2 arity2 _ methds2) ->
      -- TODO superclasses
      (name1 == name2)
        && (arity1 == arity2)
        && (methds1 == methds2)
    _ -> False

data LiteralValue
  = LNil
  | LBool !Bool
  | LString !ByteString
  | LNumber !Double
  | LCallable Callable
  | LInstance
      { _LInstanceFields :: !(IORef (Map ByteString LiteralValue))
      , _LInstanceClassName :: !ByteString
      , _LInstanceMethods :: !ClassMethodChain
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
  (LInstance fields1 parentClass1 mthds1, LInstance fields2 parentClass2 mthds2) ->
    (fields1 == fields2)
      && (parentClass1 == parentClass2)
      && (mthds1 == mthds2)
  _ -> False
