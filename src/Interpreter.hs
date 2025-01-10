{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Interpreter (evaluate, interpret) where

import Control.Monad (void, when)
import Control.Monad.Except (runExceptT, throwError, tryError)
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
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
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

evaluate :: Expr.Expr2 -> InterpreterM Expr.LiteralValue
evaluate = \case
  Expr.ELiteral val -> pure val
  Expr.ELogical left operator right -> do
    l <- evaluate left
    let cond = case operator.ttype of
          TokenType.OR -> isTruthy l
          _ -> not $ isTruthy l
    if cond then pure l else evaluate right
  Expr.ESet object name value -> do
    evObj <- evaluate object
    case evObj of
      Expr.LInstance instanceFields _ _methods -> do
        evVal <- evaluate value
        setInstanceField name evVal instanceFields
        pure evVal
      _ -> throwError $ Error.RuntimeError name "Only instances have fields."
  Expr.EThis keyword distance -> do
    lookUpVariable keyword distance
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
  Expr.ECall callee paren argumentsExprs -> do
    calleeVal <- evaluate callee
    argVals <- traverse evaluate argumentsExprs
    case calleeVal of
      Expr.LCallable c -> do
        when (c.callable_arity /= V.length argumentsExprs) $
          throwError $
            Error.RuntimeError
              paren
              ( "Expected "
                  <> BS.pack (show c.callable_arity)
                  <> " arguments but got "
                  <> BS.pack (show $ V.length argumentsExprs)
                  <> "."
              )
        c.callable_call (call c.callable_closure) argVals
      _ -> throwError $ Error.RuntimeError paren "Can only call functions and classes."
  Expr.EGet object fieldName -> do
    evaluate object >>= \case
      Expr.LInstance fields instanceName methods -> getInstanceFieldOrMethod fieldName instanceName fields methods
      _ -> throwError $ Error.RuntimeError fieldName "Only instances have properties."
  Expr.EVariable name distance -> lookUpVariable name distance
  Expr.EAssign name exprValue distance -> do
    value <- evaluate exprValue
    if distance == (-1)
      then State.gets (.globals) >>= assignFromMap name value
      else State.gets (.environment) >>= assignAt distance name value
    pure value
  where
    -- See note for Double comparison in Lox in Expr.hs
    isEqual = (==)

getInstanceFieldOrMethod
  :: TokenType.Token
  -> ByteString
  -> IORef (Map ByteString Expr.LiteralValue)
  -> ClassMethodChain
  -> InterpreterM Expr.LiteralValue
getInstanceFieldOrMethod fieldName instanceName fieldsRef classMethodChain = do
  fields <- liftIO $ readIORef fieldsRef
  case fields M.!? fieldName.lexeme of
    Just v -> pure v
    Nothing -> do
      checkMethodchain classMethodChain >>= \case
        Just foundMthd -> do
          Expr.LCallable . fst
            <$> bind fieldName fieldsRef instanceName classMethodChain foundMthd
        Nothing ->
          throwError $
            Error.RuntimeError
              fieldName
              ("Undefined property '" <> fieldName.lexeme <> "'.")
  where
    checkMethodchain :: ClassMethodChain -> InterpreterM (Maybe Expr.Callable)
    checkMethodchain = \case
      ClassNoSuper v -> common v
      ClassWithSuper v n ->
        common v >>= \case
          Nothing -> checkMethodchain n
          just -> pure just
      where
        common
          :: IORef (Map ByteString Expr.Callable) -> InterpreterM (Maybe Expr.Callable)
        common ref = liftIO (readIORef ref) >>= \m -> pure (m M.!? fieldName.lexeme)

bind
  :: TokenType.Token
  -> IORef (Map ByteString Expr.LiteralValue)
  -> ByteString
  -> ClassMethodChain
  -> Expr.Callable
  -> InterpreterM (Expr.Callable, Expr.LiteralValue)
bind fieldName fieldsRef instanceName methdsChain cf =
  case cf of
    Expr.CFunction {} -> do
      newClosureEnv <-
        liftIO (newIORef M.empty)
          <&> \mref -> LocalEnvironment mref cf.callable_closure
      let classInstance = Expr.LInstance fieldsRef instanceName methdsChain
      define "this" classInstance newClosureEnv
      pure (cf {Expr.callable_closure = newClosureEnv}, classInstance)
    _ ->
      throwError $
        Error.RuntimeError
          fieldName
          ( "Error in interpreter, field should not be class '" <> fieldName.lexeme <> "'."
          )

setInstanceField
  :: TokenType.Token
  -> Expr.LiteralValue
  -> IORef (Map ByteString Expr.LiteralValue)
  -> InterpreterM ()
setInstanceField name value mref =
  liftIO . modifyIORef' mref $
    \fieldMap -> M.insert name.lexeme value fieldMap

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
  Expr.LCallable c -> c.callable_toString
  Expr.LInstance _ klassname _ -> klassname <> " instance"

execute :: Stmt.Stmt2 -> InterpreterM ()
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
  Stmt.SClass klassName superclass _methods -> do
    superClassMethdChain <- case superclass of
      Nothing -> pure Environment.ClassNoSuper
      Just (superclassTok, dist) ->
        evaluate (Expr.EVariable superclassTok dist)
          >>= \case
            Expr.LCallable (Expr.CClass {Expr.class_methods}) -> pure $ \curr -> Environment.ClassWithSuper curr class_methods
            _ -> throwError $ Error.RuntimeError superclassTok "Superclass must be a class."
    env <- State.gets (.environment)
    define klassName.lexeme Expr.LNil env
    mayInitMethd <- liftIO $ newIORef Nothing
    thisClassMethodsChain <-
      V.foldM
        ( \acc next@(Stmt.FFunctionH fname _ _) -> do
            let isInit = fname.lexeme == "init"
            fun <- newFun next isInit
            when isInit $
              liftIO $
                writeIORef mayInitMethd (Just (fname, fun))
            pure $ M.insert fname.lexeme fun acc
        )
        M.empty
        _methods
        >>= (liftIO . newIORef)
        <&> superClassMethdChain
    mayInit <- liftIO (readIORef mayInitMethd)
    let (classArity, initMaker) = case mayInit of
          Just (initTok, originalInitMthd) -> do
            let initM fieldMapRef args = do
                  (initFunc, classInstance) <-
                    bind initTok fieldMapRef klassName.lexeme thisClassMethodsChain originalInitMthd
                  initFunc.callable_call (call initFunc.callable_closure) args
                  pure classInstance
            (originalInitMthd.callable_arity, initM)
          Nothing -> do
            let initM fieldMapRef _args =
                  pure $
                    Expr.LInstance fieldMapRef klassName.lexeme thisClassMethodsChain
            (0, initM)
    let klass =
          Expr.LCallable $
            Expr.CClass
              { Expr.callable_toString = klassName.lexeme
              , Expr.callable_arity = classArity
              , Expr.callable_call = \_evaluator args -> do
                  fieldMapRef <- liftIO $ newIORef M.empty
                  initMaker fieldMapRef args
              , Expr.class_methods = thisClassMethodsChain
              }
    assignFromMap klassName klass env.values
  Stmt.SFunction f@(Stmt.FFunctionH name _params _body) -> do
    -- environment of the function where it is declared
    function <- Expr.LCallable <$> newFun f False
    State.get >>= \st -> define name.lexeme function st.environment

newFun :: Stmt.FunctionH2 -> Bool -> InterpreterM Expr.Callable
newFun fun isInitializer = case fun of
  Stmt.FFunctionH name params body -> do
    -- environment of the function where it is declared
    closure <- State.gets (.environment)
    let function =
          Expr.CFunction
            { Expr.callable_toString = "<fn " <> name.lexeme <> ">"
            , Expr.callable_arity = V.length params
            , Expr.callable_closure = closure
            , Expr.callable_call = \evaluator args -> evaluator name body params args isInitializer
            , Expr.callable_isInitializer = isInitializer
            }
    pure function

call
  :: Environment
  -> TokenType.Token
  -> Vector Stmt.Stmt2
  -> Vector TokenType.Token
  -> Vector Expr.LiteralValue
  -> Bool
  -> InterpreterM Expr.LiteralValue
call closure funToken body params args isInitializer = do
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
  executeWithinEnvs within prevEnv funcEnvironment
  where
    within = do
      tryError (executeStmts body) >>= \case
        Left e ->
          case e of
            Error.RuntimeReturn v ->
              if isInitializer
                then execIfInit
                else pure v
            _ -> throwError e
        Right () ->
          if isInitializer
            then execIfInit
            else pure Expr.LNil
    execIfInit = do
      liftIO (readIORef closure.values) >>= \vmap ->
        case vmap M.!? "this" of
          Just thisInstance ->
            pure thisInstance
          Nothing ->
            throwError $
              Error.RuntimeError
                funToken
                "Error in interpreter, should have found reference to 'this' to return in _.init() class method."

executeBlock
  :: Vector Stmt.Stmt2 -> Environment -> Environment -> InterpreterM ()
executeBlock statements = executeWithinEnvs (executeStmts statements)

executeWithinEnvs
  :: InterpreterM a -> Environment -> Environment -> InterpreterM a
executeWithinEnvs action prevEnv tempEnv = do
  State.modify' $ \st ->
    st {environment = tempEnv}
  finallyE action $ do
    State.modify' $ \st -> st {environment = prevEnv}

interpret :: Vector Stmt.Stmt2 -> IO Error.ErrorPresent
interpret statements = do
  globalsRef <- newIORef M.empty
  writeIORef globalsRef $ globals globalsRef
  let initialInterpreterState =
        InterpreterState
          { environment = GlobalEnvironment globalsRef
          , globals = globalsRef
          }
  value <-
    State.evalStateT (runExceptT $ executeStmts statements) initialInterpreterState
  case value of
    Left e -> Error.reportRuntimeError e >> pure Error.Error
    Right () -> pure Error.NoError
  where
    globals globalsRef =
      M.fromList
        [
          ( "clock"
          , Expr.LCallable $
              Expr.CFunction
                { Expr.callable_toString = "<native fn>"
                , Expr.callable_arity = 0
                , Expr.callable_closure = GlobalEnvironment globalsRef
                , Expr.callable_isInitializer = False
                , Expr.callable_call = \_evaluator _args ->
                    -- realToFrac & fromIntegral treat NominalDiffTime as seconds
                    Expr.LNumber . realToFrac <$> liftIO Time.getPOSIXTime
                }
          )
        ]

executeStmts :: Vector Stmt.Stmt2 -> InterpreterM ()
executeStmts = traverse_ execute

recPrintEnvs
  :: Environment -> Int -> InterpreterM ()
recPrintEnvs env count = case env of
  GlobalEnvironment v -> printEnvRef v
  LocalEnvironment v enc -> printEnvRef v >> recPrintEnvs enc (count + 1)
  where
    printEnvRef :: IORef (Map ByteString Expr.LiteralValue) -> InterpreterM ()
    printEnvRef ref = do
      liftIO (readIORef ref)
        >>= liftIO . putStrLn . (\m -> show (M.map stringify m) <> " " <> show count)
