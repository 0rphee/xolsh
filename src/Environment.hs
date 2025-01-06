{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Environment where

import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class qualified as State
import Control.Monad.State.Strict (StateT)
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Error qualified
import {-# SOURCE #-} Expr qualified
import TokenType qualified

newtype InterpreterState = InterpreterState {environment :: Environment}

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
  deriving (Show)

instance Show (IORef (Map byteString Expr.LiteralValue)) where
  show _ = "iorefmap"

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
