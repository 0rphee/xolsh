{-# LANGUAGE OverloadedRecordDot #-}

module Environment
  ( InterpreterState (..)
  , Environment (..)
  , lookUpVariable
  , assignAt
  , assignFromMap
  , define
  )
where

import Bluefin.Eff
import Bluefin.Exception (Exception)
import Bluefin.Exception qualified as Exception
import Bluefin.IO (IOE, effIO)
import Bluefin.State (State)
import Bluefin.State qualified as State
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

-- type InterpreterM a =
--   ExceptT Error.RuntimeException (StateT InterpreterState IO) a

----------------------------------
--- module Environment where

data Environment
  = GlobalEnvironment {values :: IORef (Map ShortByteString Expr.LiteralValue)}
  | LocalEnvironment
      { values :: IORef (Map ShortByteString Expr.LiteralValue)
      , _enclosing :: Environment
      }
  deriving (Eq)

lookUpVariable
  :: (ex :> es, st :> es, io :> es)
  => IOE io
  -> Exception Error.RuntimeException ex
  -> State InterpreterState st
  -> TokenType.Token
  -> Int
  -> Eff es Expr.LiteralValue
lookUpVariable io ex st name distance =
  if distance == -1
    then State.get st >>= \v -> getFromMap io ex name v.globals
    else State.get st >>= \v -> getAt distance v.environment
  where
    getAt dist environment =
      case dist of
        0 -> do
          getFromMap io ex name environment.values
        _ ->
          case environment of
            GlobalEnvironment _ -> do
              -- should never happen, verified by resolver
              -- getFromMap name _mapref
              Exception.throw ex $
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
  :: forall es io ex
   . (io :> es, ex :> es)
  => IOE io
  -> Exception Error.RuntimeException ex
  -> Int
  -> TokenType.Token
  -> Expr.LiteralValue
  -> Environment
  -> Eff es ()
assignAt io ex dist name value environment = do
  mapRef <- getAncestor dist environment
  assignFromMap io ex name value mapRef
  where
    getAncestor
      :: Int
      -> Environment
      -> Eff es (IORef (Map ShortByteString Expr.LiteralValue))
    getAncestor count currEnv =
      if count == 0
        then pure currEnv.values
        else case currEnv of
          GlobalEnvironment _ -> do
            Exception.throw ex $
              Error.RuntimeError name "Failure in resolver, bug in interpreter (assignAt)."
          LocalEnvironment _ enc -> getAncestor (count - 1) enc

assignFromMap
  :: (io :> es, ex :> es)
  => IOE io
  -> Exception Error.RuntimeException ex
  -> TokenType.Token
  -> Expr.LiteralValue
  -> IORef (Map ShortByteString Expr.LiteralValue)
  -> Eff es ()
assignFromMap io ex name value mapRef =
  effIO io (readIORef mapRef) >>= \vmap ->
    if M.member name.lexeme vmap
      then effIO io $ writeIORef mapRef $ M.insert name.lexeme value vmap
      else
        Exception.throw ex $
          Error.RuntimeError
            name
            ("Undefined variable '" <> (SBS.fromShort name.lexeme) <> "'.")

getFromMap
  :: (io :> es, ex :> es)
  => IOE io
  -> Exception Error.RuntimeException ex
  -> TokenType.Token
  -> IORef (Map ShortByteString a)
  -> Eff es a
getFromMap io ex name ref =
  effIO io (readIORef ref) >>= \m -> do
    case m M.!? name.lexeme of
      Just v -> pure v
      Nothing ->
        Exception.throw ex $
          Error.RuntimeError
            name
            ("Undefined variable '" <> (SBS.fromShort name.lexeme) <> "'.")

{-# INLINEABLE define #-}
define
  :: io :> es
  => IOE io -> ShortByteString -> Expr.LiteralValue -> Environment -> Eff es ()
define io name value environment = do
  effIO io $ modifyIORef' environment.values $ \valueMap -> M.insert name value valueMap

----------------------------------
