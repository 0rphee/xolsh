{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Interpreter (evaluate, interpret) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict
  ( MonadIO (..)
  , StateT
  )
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (finallyE)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Error qualified
import Expr qualified
import Scanner (whileM)
import Stmt qualified
import TokenType qualified

{-# SPECIALIZE whileM ::
  InterpeterM Bool -> InterpeterM Bool -> InterpeterM ()
  #-}

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
get :: TokenType.Token -> InterpeterM Expr.LiteralValue
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
assign :: TokenType.Token -> Expr.LiteralValue -> InterpeterM ()
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

data InterpreterState = InterpreterState {environment :: !Environment}

{- |
Removing newtypes, @InterpreterM a@ is equivalent to:
  + @StateT InterpreterState Identitity (Either RuntimeError a)@
  + @InterpreterState -> Identitity (Either RuntimeError a, InterpreterState)@
-}
type InterpeterM a = ExceptT Error.RuntimeError (StateT InterpreterState IO) a

evaluate :: Expr.Expr -> InterpeterM Expr.LiteralValue
evaluate = \case
  Expr.ELiteral val -> pure val
  Expr.ELogical left operator right -> do
    l <- evaluate left
    let cond = case operator.ttype of
          TokenType.OR -> isTruthy l
          _ -> not $ isTruthy l
    if cond then pure l else evaluate right
  Expr.EGrouping expr -> evaluate expr
  Expr.EUnary op expr -> do
    right <- evaluate expr
    case (op.ttype, right) of
      (TokenType.BANG, val) -> pure $ Expr.LBool $ not $ isTruthy val
      (TokenType.MINUS, Expr.LNumber !v) -> pure $ Expr.LNumber (-v)
      (TokenType.MINUS, _) -> throwError $ Error.RuntimeError op "Operand must be a number."
      _ -> pure Expr.LNil -- marked as unreachable (section 7.2.3)
  Expr.EBinary lexpr op rexpr ->
    do
      left <- evaluate lexpr
      right <- evaluate rexpr
      let commonIfNumber operation =
            case (left, right) of
              (Expr.LNumber !l, Expr.LNumber !r) -> pure $ l `operation` r
              _ -> throwError $ Error.RuntimeError op "Operands must be numbers." -- TODO
      case op.ttype of
        TokenType.GREATER -> Expr.LBool <$> commonIfNumber (>)
        TokenType.GREATER_EQUAL -> Expr.LBool <$> commonIfNumber (>=)
        TokenType.LESS -> Expr.LBool <$> commonIfNumber (<)
        TokenType.LESS_EQUAL -> Expr.LBool <$> commonIfNumber (<=)
        TokenType.BANG_EQUAL -> pure $ Expr.LBool $ not $ isEqual left right
        TokenType.EQUAL_EQUAL -> pure $ Expr.LBool $ isEqual left right
        TokenType.MINUS -> Expr.LNumber <$> commonIfNumber (-)
        TokenType.PLUS ->
          case (left, right) of
            (Expr.LNumber l, Expr.LNumber r) -> pure $ Expr.LNumber $ l + r
            (Expr.LString l, Expr.LString r) -> pure $ Expr.LString $ l <> r
            _ ->
              throwError $
                Error.RuntimeError op "Operands must be two numbers or two strings."
        TokenType.SLASH -> Expr.LNumber <$> commonIfNumber (/)
        TokenType.STAR -> Expr.LNumber <$> commonIfNumber (*)
        _ -> pure Expr.LNil -- marked as unreachable (section 7.2.5)
  Expr.EVariable name -> get name
  Expr.EAssign name exprValue -> do
    value <- evaluate exprValue
    assign name value
    pure value
  where
    isEqual :: Expr.LiteralValue -> Expr.LiteralValue -> Bool
    isEqual l r = case (l, r) of
      -- Lox considers NaN equal to NaN, contrary to what (==) does (7.2.5)
      (Expr.LNumber vl, Expr.LNumber vr) | isNaN vl && isNaN vr -> True
      _ -> l == r

isTruthy :: Expr.LiteralValue -> Bool
isTruthy = \case
  Expr.LNil -> False
  Expr.LBool v -> v
  _ -> True

stringify :: Expr.LiteralValue -> ByteString
stringify = \case
  Expr.LNil -> "nil"
  Expr.LNumber v ->
    let str = BS.pack $ show v
        (pstr, end) = BS.splitAt (BS.length str - 2) str
     in if end == ".0"
          then pstr
          else str
  Expr.LBool v -> if v then "true" else "false"
  Expr.LString v -> v

execute :: Stmt.Stmt -> InterpeterM ()
execute = \case
  Stmt.SExpression expression -> void $ evaluate expression
  Stmt.SIf condition thenBranch elseBranch -> do
    c <- isTruthy <$> evaluate condition
    if c
      then execute thenBranch
      else traverse_ execute elseBranch
  Stmt.SPrint expression ->
    evaluate expression >>= \value -> liftIO $ BS.putStrLn (stringify value)
  Stmt.SVar name initializer -> do
    value <- case initializer of
      Nothing -> pure Expr.LNil
      Just v -> evaluate v
    State.modify' $ \st -> st {environment = define name.lexeme value st.environment}
  Stmt.SWhile condition body -> do
    whileM
      (isTruthy <$> evaluate condition)
      (execute body)
  Stmt.SBlock statements -> do
    executeBlock statements
    pure ()

executeBlock :: [Stmt.Stmt] -> InterpeterM ()
executeBlock statements = do
  State.modify' $ \st ->
    st
      { environment =
          LocalEnvironment {values = mempty, _enclosing = st.environment}
      }
  finallyE (executeStmts statements) $ do
    State.modify' $ \st ->
      let enc = case st.environment of
            GlobalEnvironment _ -> error "impossible" -- TODO how to remove this? GADTS
            LocalEnvironment _ e -> e
       in st {environment = enc}

interpret :: [Stmt.Stmt] -> IO Error.ErrorPresent
interpret statements = do
  value <-
    State.evalStateT (runExceptT $ executeStmts statements) initialInterpreterState
  case value of
    Left e -> Error.reportRuntimeError e >> pure Error.Error
    Right () -> pure Error.NoError
  where
    initialInterpreterState =
      InterpreterState
        { environment = GlobalEnvironment {values = mempty}
        }

executeStmts :: [Stmt.Stmt] -> InterpeterM ()
executeStmts = \case
  [] -> pure ()
  (stmt : stmts) -> execute stmt >> executeStmts stmts
