{-# LANGUAGE OverloadedRecordDot #-}

module Environment
  ( InterpreterState (..)
  , ValueMap
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
import Data.ByteString.Short qualified as SBS
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Error qualified
import {-# SOURCE #-} Expr qualified
import TokenType qualified

type ValueMap = IntMap Expr.LiteralValue

data InterpreterState = InterpreterState
  { globals :: IORef ValueMap
  , environment :: Environment
  }

data Environment
  = GlobalEnvironment {values :: IORef ValueMap}
  | LocalEnvironment
      { values :: IORef ValueMap
      , _enclosing :: Environment
      }
  deriving (Eq)

lookUpVariable
  :: (ex :> es, st :> es, io :> es)
  => IOE io
  -> Exception Error.RuntimeException ex
  -> State InterpreterState st
  -> TokenType.Token
  -> Expr.AccessInfo
  -> Eff es Expr.LiteralValue
lookUpVariable io ex st name accessInfo =
  if (accessInfo.distance == (-1))
    then State.get st >>= \v -> getFromMap io ex name accessInfo.index v.globals
    else State.get st >>= \v -> getAt accessInfo.distance v.environment
  where
    getAt dist environment =
      case dist of
        0 -> do
          getFromMap io ex name accessInfo.index environment.values
        _ ->
          case environment of
            GlobalEnvironment _ -> do
              -- should never happen, verified by resolver
              -- getFromMap name _mapref
              Exception.throw ex $
                Error.RuntimeError
                  name.tline
                  ( "Failure in resolver, bug in interpreter (lookUpVariable)."
                      <> B.pack (show name)
                      <> " "
                      <> B.pack (show accessInfo)
                      <> "."
                  )
            LocalEnvironment _ enc -> getAt (dist - 1) enc

assignAt
  :: forall es io ex
   . (io :> es, ex :> es)
  => IOE io
  -> Exception Error.RuntimeException ex
  -> Expr.AccessInfo
  -> TokenType.Token
  -> Expr.LiteralValue
  -> Environment
  -> Eff es ()
assignAt io ex accessInfo name value environment = do
  mapRef <- getAncestor accessInfo.distance environment
  assignFromMap io ex name accessInfo value mapRef
  where
    getAncestor
      :: Int
      -> Environment
      -> Eff es (IORef ValueMap)
    getAncestor count currEnv =
      if count == 0
        then pure currEnv.values
        else case currEnv of
          GlobalEnvironment _ -> do
            Exception.throw ex $
              Error.RuntimeError
                name.tline
                "Failure in resolver, bug in interpreter (assignAt)."
          LocalEnvironment _ enc -> getAncestor (count - 1) enc

assignFromMap
  :: (io :> es, ex :> es)
  => IOE io
  -> Exception Error.RuntimeException ex
  -> TokenType.Token
  -> Expr.AccessInfo
  -> Expr.LiteralValue
  -> IORef ValueMap
  -> Eff es ()
assignFromMap io ex name accessInfo value mapRef =
  effIO io (readIORef mapRef) >>= \vmap ->
    if M.member accessInfo.index vmap
      then effIO io $ writeIORef mapRef $ M.insert accessInfo.index value vmap
      else
        Exception.throw ex $
          Error.RuntimeError
            name.tline
            ("Undefined variable '" <> (SBS.fromShort name.lexeme) <> "'.")

getFromMap
  :: (io :> es, ex :> es)
  => IOE io
  -> Exception Error.RuntimeException ex
  -> TokenType.Token
  -> Int
  -> IORef ValueMap
  -> Eff es Expr.LiteralValue
getFromMap io ex name accessInfoIndex ref =
  effIO io (readIORef ref) >>= \m -> do
    case m M.!? accessInfoIndex of
      Just v -> pure v
      Nothing ->
        Exception.throw ex $
          Error.RuntimeError
            name.tline
            ("Undefined variable '" <> (SBS.fromShort name.lexeme) <> "'.")

{-# INLINEABLE define #-}
define
  :: io :> es
  => IOE io -> Int -> Expr.LiteralValue -> Environment -> Eff es ()
define io accessIndex value environment = do
  effIO io $ modifyIORef' environment.values $ \valueMap -> M.insert accessIndex value valueMap

----------------------------------
