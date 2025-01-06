{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Interpreter (evaluate, interpret) where

import Control.Monad (void, when)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State.Strict
  ( MonadIO (..)
  )
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (finallyE)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as V
import Environment
import Error qualified
import Expr qualified
import Scanner (whileM)
import Stmt qualified
import TokenType qualified

{-# SPECIALIZE whileM ::
  InterpreterM Bool -> InterpreterM a -> InterpreterM ()
  #-}

evaluate :: Expr.Expr -> InterpreterM Expr.LiteralValue
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
  Expr.ECall callee paren arguments -> do
    calleeVal <- evaluate callee
    argVals <- traverse evaluate arguments
    case calleeVal of
      Expr.LCallable arity call _ -> do
        when (arity /= V.length arguments) $
          throwError $
            Error.RuntimeError
              paren
              ( "Expected "
                  <> BS.pack (show arity)
                  <> " arguments but got "
                  <> BS.pack (show $ V.length arguments)
                  <> "."
              )
        call argVals
      _ -> throwError $ Error.RuntimeError paren "Can only call functions and classes."
  Expr.EVariable name -> get name
  Expr.EAssign name exprValue -> do
    value <- evaluate exprValue
    assign name value
    pure value
  where
    -- See note for Double comparison in Lox in Expr.hs
    isEqual = (==)

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
  Expr.LCallable _ _ toString -> toString

execute :: Stmt.Stmt -> InterpreterM ()
execute = \case
  Stmt.SExpression expression -> void $ evaluate expression
  Stmt.SIf condition thenBranch elseBranch -> do
    c <- isTruthy <$> evaluate condition
    if c
      then execute thenBranch
      else traverse_ execute elseBranch
  Stmt.SPrint expression ->
    evaluate expression >>= \value -> liftIO $ BS.putStrLn (stringify value)
  Stmt.SReturn _ valueExpr -> do
    value <- fromMaybe Expr.LNil <$> traverse evaluate valueExpr
    throwError $ Error.RuntimeReturn value
  Stmt.SVar name initializer -> do
    value <- case initializer of
      Nothing -> pure Expr.LNil
      Just v -> evaluate v
    State.get >>= \st -> define name.lexeme value st.environment
  Stmt.SWhile condition body -> do
    whileM
      (isTruthy <$> evaluate condition)
      (execute body)
  Stmt.SBlock statements -> do
    newEnvValueMapRef <- liftIO $ newIORef mempty
    prevEnv <- State.gets (.environment)
    executeBlock statements prevEnv (LocalEnvironment newEnvValueMapRef prevEnv)
    pure ()
  Stmt.SFunction name params body -> do
    -- environment of the function where it is declared
    closure <- State.gets (.environment)
    let function =
          Expr.LCallable
            { Expr.callable_toString = "<fn " <> name.lexeme <> ">"
            , Expr.callable_arity = V.length params
            , Expr.callable_call = \args -> do
                -- environment of the function before being called
                prevEnv <- State.gets (.environment)
                -- local environment of the function with arguments paired with parameters
                funcEnvironment <-
                  V.zipWith (\tok -> (tok.lexeme,)) params args
                    & V.toList
                    & M.fromList
                    & newIORef
                    & liftIO
                    <&> (`LocalEnvironment` closure)
                -- if the function does not return via a return stmt, it will default to nil
                catchError
                  ( executeBlock body prevEnv funcEnvironment >> pure Expr.LNil
                  )
                  $ \case
                    Error.RuntimeReturn value -> pure value
                    e -> throwError e
            }
    State.get >>= \st -> define name.lexeme function st.environment

executeBlock
  :: Vector Stmt.Stmt -> Environment -> Environment -> InterpreterM ()
executeBlock statements prevEnv tempEnv = do
  State.modify' $ \st ->
    st {environment = tempEnv}
  finallyE (executeStmts statements) $ do
    State.modify' $ \st -> st {environment = prevEnv}

interpret :: Vector Stmt.Stmt -> IO Error.ErrorPresent
interpret statements = do
  globalsRef <- newIORef globals
  let initialInterpreterState =
        InterpreterState
          { environment = GlobalEnvironment {values = globalsRef}
          }
  value <-
    State.evalStateT (runExceptT $ executeStmts statements) initialInterpreterState
  case value of
    Left e -> Error.reportRuntimeError e >> pure Error.Error
    Right () -> pure Error.NoError
  where
    globals =
      M.fromList
        [
          ( "clock"
          , Expr.LCallable
              { Expr.callable_toString = "<native fn>"
              , Expr.callable_arity = 0
              , Expr.callable_call = \_args ->
                  -- realToFrac & fromIntegral treat NominalDiffTime as seconds
                  Expr.LNumber . realToFrac <$> liftIO Time.getPOSIXTime
              }
          )
        ]

executeStmts :: Vector Stmt.Stmt -> InterpreterM ()
executeStmts s = traverse_ execute s --  >> ((State.gets (.environment)) >>= liftIO . print)
