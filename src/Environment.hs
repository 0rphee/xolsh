{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Environment
  ( InterpreterState (..)
  , InterpreterM
  , Environment (..)
  , ClassMethodChain (..)
  , lookUpVariable
  , assignAt
  , assignFromMap
  , define
  , checkMethodChain
  )
where

import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class qualified as State
import Control.Monad.State.Strict (StateT)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Error qualified
import {-# SOURCE #-} Expr qualified
import TokenType qualified

data InterpreterState = InterpreterState
  { globals :: IORef (Map ByteString Expr.LiteralValue)
  , environment :: Environment
  }

{- |
Removing newtypes, @InterpreterM a@ is equivalent to:
  + @StateT InterpreterState Identitity (Either RuntimeError a)@
  + @InterpreterState -> Identitity (Either RuntimeError a, InterpreterState)@
-}
type InterpreterM a =
  ExceptT Error.RuntimeException (StateT InterpreterState IO) a

----------------------------------
--- module Environment where

data Environment
  = GlobalEnvironment {values :: IORef (Map ByteString Expr.LiteralValue)}
  | LocalEnvironment
      { values :: IORef (Map ByteString Expr.LiteralValue)
      , _enclosing :: Environment
      }
  deriving (Show, Eq)

data ClassMethodChain
  = ClassNoSuper {this_methods :: IORef (Map ByteString Expr.Callable)}
  | ClassWithSuper
      { this_methods :: IORef (Map ByteString Expr.Callable)
      , _superMethods :: ClassMethodChain
      }
  deriving (Eq)

instance Show (IORef (Map ByteString Expr.LiteralValue)) where
  show _ = "iorefmap"

lookUpVariable :: TokenType.Token -> Int -> InterpreterM Expr.LiteralValue
lookUpVariable name distance =
  if distance == -1
    then State.gets (.globals) >>= getFromMap name
    else State.gets (.environment) >>= getAt distance
  where
    getAt dist environment =
      case dist of
        0 -> do
          getFromMap name environment.values
        _ ->
          case environment of
            GlobalEnvironment _ -> do
              -- should never happen, verified by resolver
              -- getFromMap name _mapref
              throwError $
                Error.RuntimeError
                  name
                  ( "Failure in resolver, bug in interpreter (lookUpVariable)."
                      <> B.pack (show name)
                      <> " "
                      <> B.pack (show distance)
                      <> "."
                  )
            LocalEnvironment _ enc -> getAt (dist - 1) enc

checkMethodChain
  :: ByteString -> ClassMethodChain -> InterpreterM (Maybe Expr.Callable)
checkMethodChain fieldName = go
  where
    go = \case
      ClassNoSuper v -> common v
      ClassWithSuper v n ->
        common v >>= \case
          Nothing -> go n
          just -> pure just
      where
        common
          :: IORef (Map ByteString Expr.Callable) -> InterpreterM (Maybe Expr.Callable)
        common ref = liftIO (readIORef ref) >>= \m -> pure (m M.!? fieldName)

assignAt
  :: Int -> TokenType.Token -> Expr.LiteralValue -> Environment -> InterpreterM ()
assignAt dist name value environment = do
  mapRef <- getAncestor dist environment
  assignFromMap name value mapRef
  where
    getAncestor
      :: Int -> Environment -> InterpreterM (IORef (Map ByteString Expr.LiteralValue))
    getAncestor count currEnv =
      if count == 0
        then pure currEnv.values
        else case currEnv of
          GlobalEnvironment _ -> do
            throwError $
              Error.RuntimeError name "Failure in resolver, bug in interpreter (assignAt)."
          LocalEnvironment _ enc -> getAncestor (count - 1) enc

assignFromMap
  :: TokenType.Token
  -> Expr.LiteralValue
  -> IORef (Map ByteString Expr.LiteralValue)
  -> InterpreterM ()
assignFromMap name value mapRef =
  liftIO (readIORef mapRef) >>= \vmap ->
    if M.member name.lexeme vmap
      then liftIO $ writeIORef mapRef $ M.insert name.lexeme value vmap
      else
        throwError $
          Error.RuntimeError name ("Undefined variable '" <> name.lexeme <> "'.")

getFromMap
  :: TokenType.Token
  -> IORef (Map ByteString a)
  -> InterpreterM a
getFromMap name ref =
  liftIO (readIORef ref) >>= \m -> do
    case m M.!? name.lexeme of
      Just v -> pure v
      Nothing ->
        throwError $
          Error.RuntimeError name ("Undefined variable '" <> name.lexeme <> "'.")

{-# INLINEABLE define #-}
define
  :: ByteString -> Expr.LiteralValue -> Environment -> InterpreterM ()
define name value environment = do
  liftIO $ modifyIORef' environment.values $ \valueMap -> M.insert name value valueMap

{-# INLINEABLE get #-}
get :: TokenType.Token -> InterpreterM Expr.LiteralValue
get name = do
  state <- State.get
  envget state.environment >>= \case
    Just v -> pure v
    Nothing ->
      throwError $
        Error.RuntimeError name ("Undefined variable '" <> name.lexeme <> "'.")
  where
    envget :: Environment -> InterpreterM (Maybe Expr.LiteralValue)
    envget env =
      liftIO (readIORef env.values) >>= \valueMap ->
        case valueMap M.!? name.lexeme of
          Nothing ->
            case env of
              LocalEnvironment _ enc -> envget enc
              _ -> pure Nothing
          v -> pure v

{-# INLINEABLE assign #-}
assign :: TokenType.Token -> Expr.LiteralValue -> InterpreterM ()
assign name value = do
  -- there's no implicit variable declaration like in python
  state <- State.get
  envassign state.environment >>= \case
    True -> pure ()
    False ->
      throwError $
        Error.RuntimeError name ("Undefined variable '" <> name.lexeme <> "'.")
  where
    -- True if successful assignment, False otherwise
    envassign :: Environment -> InterpreterM Bool
    envassign env = do
      liftIO (readIORef env.values) >>= \vals ->
        if M.member name.lexeme vals
          then define name.lexeme value env >> pure True
          else case env of
            GlobalEnvironment _ -> pure False
            LocalEnvironment _ enclosing -> envassign enclosing

----------------------------------
