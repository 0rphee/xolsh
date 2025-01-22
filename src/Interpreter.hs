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
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock.POSIX qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as V
import Environment
import Error qualified
import Expr qualified
import Numeric qualified
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
      Expr.LInstance instanceFields _methods -> do
        evVal <- evaluate value
        setInstanceField name evVal instanceFields
        pure evVal
      _ -> throwError $ Error.RuntimeError name "Only instances have fields."
  Expr.ESuper keyword superDist methdName -> do
    superclass <-
      lookUpVariable keyword superDist >>= \case
        Expr.LCallable (Expr.CClass c) -> pure c
        _ ->
          throwError $
            Error.RuntimeError
              methdName
              "Error in interpreter, looked for 'super' and found something different to a class"

    lookUpVariable
      (TokenType.Token TokenType.THIS "this" keyword.tline)
      (superDist - 1)
      >>= \case
        Expr.LInstance fields thisInstanceClass -> do
          checkMethodChain methdName.lexeme superclass >>= \case
            Nothing ->
              throwError $
                Error.RuntimeError
                  methdName
                  ("Undefined property '" <> methdName.lexeme <> "'.")
            Just fun ->
              Expr.LCallable . Expr.CFunction . fst
                <$> bind fields thisInstanceClass fun
        -- TODO what if it doesnt find afunction
        _ ->
          throwError $
            Error.RuntimeError
              keyword
              "Error in interpreter, bug while looking up 'this' for a 'super' call."
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
        (callable_arity, callable_call) <- case c of
          Expr.CClass ref -> do
            liftIO (readIORef ref)
              <&> (\cv -> (cv.class_arity, cv.class_call ref argVals))
          Expr.CFunction f ->
            pure
              ( f.fun_arity
              , f.fun_call (call f.fun_closure) argVals
              )
        when (callable_arity /= V.length argumentsExprs) $
          throwError $
            Error.RuntimeError
              paren
              ( "Expected "
                  <> BS.pack (show callable_arity)
                  <> " arguments but got "
                  <> BS.pack (show $ V.length argumentsExprs)
                  <> "."
              )
        callable_call
      _ -> throwError $ Error.RuntimeError paren "Can only call functions and classes."
  Expr.EGet object fieldName -> do
    evaluate object >>= \case
      Expr.LInstance fields methods -> getInstanceFieldOrMethod fieldName fields methods
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
  -> IORef (Map ByteString Expr.LiteralValue)
  -> IORef Expr.LoxRuntimeClass
  -> InterpreterM Expr.LiteralValue
getInstanceFieldOrMethod fieldName fieldsRef classSuperclass = do
  fields <- liftIO $ readIORef fieldsRef
  case fields M.!? fieldName.lexeme of
    Just v -> pure v
    Nothing -> do
      checkMethodChain fieldName.lexeme classSuperclass >>= \case
        Just foundMthd -> do
          Expr.LCallable . Expr.CFunction . fst
            <$> bind fieldsRef classSuperclass foundMthd
        -- TODO fix for when
        Nothing ->
          throwError $
            Error.RuntimeError
              fieldName
              ("Undefined property '" <> fieldName.lexeme <> "'.")

bind
  :: IORef (Map ByteString Expr.LiteralValue)
  -> IORef Expr.LoxRuntimeClass
  -> Expr.LoxRuntimeFunction
  -> InterpreterM (Expr.LoxRuntimeFunction, Expr.LiteralValue)
bind fieldsRef classOfInstance runtimeFun = do
  newClosureEnv <-
    liftIO (newIORef M.empty)
      <&> \mref -> LocalEnvironment mref runtimeFun.fun_closure
  let classInstance = Expr.LInstance fieldsRef classOfInstance
  define "this" classInstance newClosureEnv
  pure (runtimeFun {Expr.fun_closure = newClosureEnv}, classInstance) -- TODO

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

stringify :: Expr.LiteralValue -> InterpreterM ByteString
stringify = \case
  Expr.LNil -> pure "nil"
  Expr.LNumber v ->
    -- see https://docs.oracle.com/javase/8/docs/api/java/lang/Double.html#toString-double-
    -- not exactly the same as jlox due to Double.toString() semantics, but this satisfies the jlox tests
    pure $
      let str = BS.pack $ Numeric.showFFloat Nothing v ""
          (pstr, end) = BS.splitAt (BS.length str - 2) str
       in if end == ".0"
            then pstr
            else str
  Expr.LBool v -> pure $ if v then "true" else "false"
  Expr.LString v -> pure v
  Expr.LCallable (Expr.CClass cref) -> liftIO (readIORef cref) <&> (.class_name)
  Expr.LCallable (Expr.CFunction f) -> pure f.fun_toString
  Expr.LInstance _ cref -> do
    cname <- liftIO (readIORef cref) <&> (.class_name)
    pure $ cname <> " instance"

execute :: Stmt.Stmt2 -> InterpreterM ()
execute = \case
  Stmt.SExpression expression -> void $ evaluate expression
  Stmt.SIf condition thenBranch elseBranch -> do
    c <- isTruthy <$> evaluate condition
    if c
      then execute thenBranch
      else traverse_ execute elseBranch
  Stmt.SPrint expression ->
    evaluate expression >>= stringify >>= liftIO . BS.putStrLn
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
  Stmt.SClass klassName maySuperClassInfo _methods -> do
    origEnv <- State.gets (.environment)
    define klassName.lexeme Expr.LNil origEnv
    maySuperClass <- case maySuperClassInfo of
      Nothing -> pure Nothing
      Just (superclassTok, dist) -> do
        evaluate (Expr.EVariable superclassTok dist)
          >>= \case
            c@(Expr.LCallable (Expr.CClass superclassRef)) -> do
              oldEnv <- State.gets (.environment)
              newEnv <- do
                superEnvRef <- liftIO $ newIORef M.empty
                pure $ LocalEnvironment superEnvRef oldEnv
              State.modify' $ \st -> st {environment = newEnv}
              define "super" c newEnv
              pure $ Just superclassRef
            _ -> throwError $ Error.RuntimeError superclassTok "Superclass must be a class."
    (mayInitMethdThisClass, thisClassMethods) <-
      V.foldM
        ( \(prevMayInit, acc) next@(Stmt.FFunctionH fname _ _) -> do
            let isInit = fname.lexeme == "init"
            fun <- newFun next isInit
            let mayInit = if isInit then Just fun else prevMayInit
            pure (mayInit, M.insert fname.lexeme fun acc)
        )
        (Nothing, M.empty)
        _methods
        >>= traverse (liftIO . newIORef)
    mayInitWSuper <-
      case mayInitMethdThisClass of
        Nothing -> do
          case maySuperClass of
            Nothing -> pure Nothing
            Just superClass ->
              checkMethodChain "init" superClass >>= \case
                Just superInitMthd -> pure $ Just superInitMthd
                Nothing -> pure Nothing
        just -> do pure just
    when (isJust maySuperClass) $ State.modify' $ \st -> st {environment = st.environment._enclosing} -- restore "super" binding
    let (classArity, initMaker) = case mayInitWSuper of
          Just originalInitMthd -> do
            let initM fieldMapRef thisClassRef args = do
                  (initFunc, classInstance) <-
                    bind fieldMapRef thisClassRef originalInitMthd
                  initFunc.fun_call (call initFunc.fun_closure) args
                  pure classInstance
            (originalInitMthd.fun_arity, initM)
          Nothing -> do
            let initM fieldMapRef thisClassRef _args =
                  pure $ Expr.LInstance fieldMapRef thisClassRef
            (0, initM)
    klass <-
      liftIO $
        Expr.LCallable . Expr.CClass
          <$> ( newIORef $
                  Expr.LRClass
                    { Expr.class_name = klassName.lexeme
                    , Expr.class_arity = classArity
                    , Expr.class_call = \thisClassRef args -> do
                        fieldMapRef <- liftIO $ newIORef M.empty
                        initMaker fieldMapRef thisClassRef args
                    , Expr.class_methods = thisClassMethods
                    , Expr.class_superclass = maySuperClass
                    }
              )

    assignFromMap klassName klass origEnv.values
  Stmt.SFunction f@(Stmt.FFunctionH name _params _body) -> do
    -- environment of the function where it is declared
    function <- Expr.LCallable . Expr.CFunction <$> newFun f False
    State.get >>= \st -> define name.lexeme function st.environment

newFun :: Stmt.FunctionH2 -> Bool -> InterpreterM Expr.LoxRuntimeFunction
newFun fun isInitializer = case fun of
  Stmt.FFunctionH name params body -> do
    -- environment of the function where it is declared
    closure <- State.gets (.environment)
    let function =
          Expr.LRFunction
            { Expr.fun_toString = "<fn " <> name.lexeme <> ">"
            , Expr.fun_arity = V.length params
            , Expr.fun_closure = closure
            , Expr.fun_call = \evaluator args -> evaluator name body params args isInitializer
            , Expr.fun_isInitializer = isInitializer
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
              Expr.CFunction $
                Expr.LRFunction
                  { Expr.fun_toString = "<native fn>"
                  , Expr.fun_arity = 0
                  , Expr.fun_closure = GlobalEnvironment globalsRef
                  , Expr.fun_isInitializer = False
                  , Expr.fun_call = \_evaluator _args ->
                      -- realToFrac & fromIntegral treat NominalDiffTime as seconds
                      Expr.LNumber . realToFrac <$> liftIO Time.getPOSIXTime
                  }
          )
        ,
          ( "read"
          , Expr.LCallable $
              Expr.CFunction $
                Expr.LRFunction
                  { Expr.fun_toString = "<native fn>"
                  , Expr.fun_arity = 0
                  , Expr.fun_closure = GlobalEnvironment globalsRef
                  , Expr.fun_isInitializer = False
                  , Expr.fun_call = \_evaluator _args ->
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
      mapv <- liftIO (readIORef ref)
      stringMap <- traverse stringify mapv
      liftIO . putStrLn $ show stringMap <> " " <> show count

checkMethodChain
  :: ByteString
  -> IORef Expr.LoxRuntimeClass
  -> InterpreterM (Maybe Expr.LoxRuntimeFunction)
checkMethodChain fieldName = go
  where
    go :: IORef Expr.LoxRuntimeClass -> InterpreterM (Maybe Expr.LoxRuntimeFunction)
    go ref = do
      klass <- liftIO (readIORef ref)
      checkMethods klass >>= \case
        Just fun -> pure $ Just fun
        Nothing -> case klass.class_superclass of
          Nothing -> pure Nothing
          Just supref -> go supref
      where
        checkMethods
          :: Expr.LoxRuntimeClass
          -> InterpreterM (Maybe Expr.LoxRuntimeFunction)
        checkMethods klass = do
          vmap <- liftIO (readIORef klass.class_methods)
          pure (vmap M.!? fieldName)
