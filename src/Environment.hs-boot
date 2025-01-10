module Environment where

import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (StateT)
import {-# SOURCE #-} Error qualified

data Environment

type InterpreterM a =
  ExceptT Error.RuntimeException (StateT InterpreterState IO) a

data InterpreterState
