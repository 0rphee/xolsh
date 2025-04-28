{-# LANGUAGE OverloadedRecordDot #-}

module Environment
  ( InterpreterState (..)
  , InterpreterM
  , Environment (..)
  , lookUpVariable
  , assignAt
  , assignFromMap
  , define
  )
where

import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class qualified as State
import Control.Monad.State.Strict (StateT)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Error qualified
import {-# SOURCE #-} Expr qualified
import TokenType qualified

data InterpreterState = InterpreterState
  { globals :: IORef (Map ShortByteString Expr.LiteralValue)
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
  = GlobalEnvironment {values :: IORef (Map ShortByteString Expr.LiteralValue)}
  | LocalEnvironment
      { values :: IORef (Map ShortByteString Expr.LiteralValue)
      , _enclosing :: Environment
      }
  deriving (Eq)

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

assignAt
  :: Int -> TokenType.Token -> Expr.LiteralValue -> Environment -> InterpreterM ()
assignAt dist name value environment = do
  mapRef <- getAncestor dist environment
  assignFromMap name value mapRef
  where
    getAncestor
      :: Int
      -> Environment
      -> InterpreterM (IORef (Map ShortByteString Expr.LiteralValue))
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
  -> IORef (Map ShortByteString Expr.LiteralValue)
  -> InterpreterM ()
assignFromMap name value mapRef =
  liftIO (readIORef mapRef) >>= \vmap ->
    if M.member name.lexeme vmap
      then liftIO $ writeIORef mapRef $ M.insert name.lexeme value vmap
      else
        throwError $
          Error.RuntimeError
            name
            ("Undefined variable '" <> (SBS.fromShort name.lexeme) <> "'.")

getFromMap
  :: TokenType.Token
  -> IORef (Map ShortByteString a)
  -> InterpreterM a
getFromMap name ref =
  liftIO (readIORef ref) >>= \m -> do
    case m M.!? name.lexeme of
      Just v -> pure v
      Nothing ->
        throwError $
          Error.RuntimeError
            name
            ("Undefined variable '" <> (SBS.fromShort name.lexeme) <> "'.")

{-# INLINEABLE define #-}
define
  :: ShortByteString -> Expr.LiteralValue -> Environment -> InterpreterM ()
define name value environment = do
  liftIO $ modifyIORef' environment.values $ \valueMap -> M.insert name value valueMap

----------------------------------
