{-# LANGUAGE OverloadedRecordDot #-}

module Environment where

import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.State.Class qualified as State
import Control.Monad.State.Strict (StateT)
import Data.ByteString.Char8 (ByteString)
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
  = GlobalEnvironment
      {values :: Map ByteString Expr.LiteralValue}
  | LocalEnvironment
      { values :: Map ByteString Expr.LiteralValue
      , _enclosing :: Environment -- DO NOT USE
      }

{-# INLINEABLE define #-}
define :: ByteString -> Expr.LiteralValue -> Environment -> Environment
define name value environment =
  environment {values = M.insert name value environment.values}

{-# INLINEABLE get #-}
get :: TokenType.Token -> InterpreterM Expr.LiteralValue
get name = do
  state <- State.get
  case envget state.environment of
    Just v -> pure v
    Nothing ->
      throwError $
        Error.RuntimeError name ("Undefined variable '" <> name.lexeme <> "'.")
  where
    envget :: Environment -> Maybe Expr.LiteralValue
    envget env =
      case env.values M.!? name.lexeme of
        Nothing ->
          case env of
            LocalEnvironment _ enc -> envget enc
            _ -> Nothing
        v -> v

{-# INLINEABLE assign #-}
assign :: TokenType.Token -> Expr.LiteralValue -> InterpreterM ()
assign name value = do
  -- there's no implicit variable declaration like in python
  state <- State.get
  case envassign state.environment of
    Just newEnv ->
      -- we update the interpreter state
      State.put $ state {environment = newEnv}
    Nothing ->
      throwError $
        Error.RuntimeError name ("Undefined variable '" <> name.lexeme <> "'.")
  where
    envassign :: Environment -> Maybe Environment
    envassign env =
      if M.member name.lexeme env.values
        then Just $ define name.lexeme value env
        else case env of
          GlobalEnvironment _ -> Nothing
          LocalEnvironment values enclosing ->
            LocalEnvironment values <$> envassign enclosing

----------------------------------
