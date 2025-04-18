{-# LANGUAGE LambdaCase #-}
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
import Data.Vector.Mutable qualified as MV
import Error qualified
import {-# SOURCE #-} Expr qualified
import TokenType qualified

data InterpreterState = InterpreterState
  { globals :: IORef (MV.IOVector Expr.LiteralValue)
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
  = GlobalEnvironment {values :: IORef (MV.IOVector Expr.LiteralValue)}
  | LocalEnvironment
      { values :: IORef (MV.IOVector Expr.LiteralValue)
      , _enclosing :: Environment
      }
  deriving (Eq)

lookUpVariable
  :: TokenType.Token -> Expr.NameInfo -> InterpreterM Expr.LiteralValue
lookUpVariable name nameInfo =
  if nameInfo.nameInfo_scope == -1
    then State.gets (.globals) >>= getFromMap name nameInfo.nameInfo_index
    else State.gets (.environment) >>= getAt nameInfo.nameInfo_scope
  where
    getAt :: Int -> Environment -> InterpreterM Expr.LiteralValue
    getAt scopeDist environment =
      case scopeDist of
        0 -> do
          getFromMap name nameInfo.nameInfo_index environment.values
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
                      <> B.pack (show nameInfo)
                      <> "."
                  )
            LocalEnvironment _ enc -> getAt (scopeDist - 1) enc

assignAt
  :: Expr.NameInfo
  -> TokenType.Token
  -> Expr.LiteralValue
  -> Environment
  -> InterpreterM ()
assignAt nameInfo name value environment = do
  mapRef <- getAncestor nameInfo.nameInfo_scope environment
  assignFromMap name nameInfo.nameInfo_index value mapRef
  where
    getAncestor
      :: Int -> Environment -> InterpreterM (IORef (MV.IOVector Expr.LiteralValue))
    getAncestor !count !currEnv =
      if count == 0
        then pure currEnv.values
        else case currEnv of
          GlobalEnvironment _ -> do
            throwError $
              Error.RuntimeError name "Failure in resolver, bug in interpreter (assignAt)."
          LocalEnvironment _ enc -> getAncestor (count - 1) enc

assignFromMap
  :: TokenType.Token
  -> Int
  -> Expr.LiteralValue
  -> IORef (MV.IOVector Expr.LiteralValue)
  -> InterpreterM ()
assignFromMap name ix value mapRef =
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
  -> Int
  -> IORef (MV.IOVector a)
  -> InterpreterM a
getFromMap name ix ref = do
  vec <- liftIO (readIORef ref)
  liftIO (MV.readMaybe vec ix) >>= \case
    Just v -> pure v
    Nothing ->
      throwError $
        Error.RuntimeError name ("Undefined variable '" <> name.lexeme <> "'.")

{-# INLINEABLE define #-}
define
  :: ByteString
  -> Expr.NameInfo
  -> Expr.LiteralValue
  -> Environment
  -> InterpreterM ()
define name nameInfo value environment = do
  liftIO $
    readIORef environment.values >>= \ref -> MV.write ref nameInfo.nameInfo_index value

--  liftIO $ modifyIORef' environment.values $ \valueMap -> M.insert name value valueMap
