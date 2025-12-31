{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Interpreter (evaluate, interpret) where

import Bluefin.EarlyReturn (EarlyReturn)
import Bluefin.EarlyReturn qualified as EarlyReturn
import Bluefin.Eff
import Bluefin.Exception (Exception)
import Bluefin.Exception qualified as Exception
import Bluefin.IO (IOE, effIO)
import Bluefin.State (State)
import Bluefin.State qualified as State
import Control.Monad (void, when)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Hashable qualified as Hashable
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock.POSIX qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word8)
import Environment
import Error qualified
import Expr qualified
import Foreign (Ptr, free, mallocBytes, peek)
import Numeric qualified
import Scanner (whileM)
import Stmt qualified
import System.Exit qualified
import System.IO (Handle, hGetBuf, stderr, stdin)
import TokenType qualified

evaluate ::
  forall es io ex st ret.
  (io :> es, ex :> es, st :> es, ret :> es) =>
  IOE io ->
  Exception Error.RuntimeException ex ->
  State InterpreterState st ->
  EarlyReturn Expr.LiteralValue ret ->
  Expr.Expr2 ->
  Eff es Expr.LiteralValue
evaluate io ex st ret = \case
  Expr.ELiteral val -> pure val
  Expr.ELogical left operator right -> do
    l <- evaluate io ex st ret left
    let cond = case operator.ttype of
          TokenType.OR -> isTruthy l
          _ -> not $ isTruthy l
    if cond then pure l else evaluate io ex st ret right
  Expr.ESet object name value -> do
    evObj <- evaluate io ex st ret object
    case evObj of
      Expr.LInstance instanceFields _methods -> do
        evVal <- evaluate io ex st ret value
        setInstanceField io name evVal instanceFields
        pure evVal
      _ ->
        Exception.throw ex $ Error.RuntimeError name.tline "Only instances have fields."
  Expr.ESuper keyword accessInfo methdName -> do
    superclass <-
      lookUpVariable io ex st keyword accessInfo >>= \case
        Expr.LCallable (Expr.CClass c) -> pure c
        _ ->
          Exception.throw ex $
            Error.RuntimeError
              methdName.tline
              "Error in interpreter, looked for 'super' and found something different to a class"

    lookUpVariable
      io
      ex
      st
      (TokenType.Token TokenType.THIS "this" keyword.tline)
      ( Expr.MkAccessInfo
          { Expr.distance = accessInfo.distance - 1,
            Expr.index = thisHash
          }
      )
      >>= \case
        Expr.LInstance fields thisInstanceClass -> do
          checkMethodChain io methdName.lexeme superclass >>= \case
            Nothing ->
              Exception.throw ex $
                Error.RuntimeError
                  methdName.tline
                  ("Undefined property '" <> SBS.fromShort methdName.lexeme <> "'.")
            Just fun ->
              Expr.LCallable . Expr.CFunction . fst
                <$> bind io fields thisInstanceClass fun
        -- TODO what if it doesnt find afunction
        _ ->
          Exception.throw ex $
            Error.RuntimeError
              keyword.tline
              "Error in interpreter, bug while looking up 'this' for a 'super' call."
  Expr.EThis keyword distance -> do
    lookUpVariable io ex st keyword distance
  Expr.EUnary op expr -> do
    right <- evaluate io ex st ret expr
    case (op.ttype, right) of
      (TokenType.BANG, val) -> pure $ Expr.LBool $ not $ isTruthy val
      (TokenType.MINUS, Expr.LNumber !v) -> pure $ Expr.LNumber (-v)
      (TokenType.MINUS, _) -> Exception.throw ex $ Error.RuntimeError op.tline "Operand must be a number."
      _ -> pure Expr.LNil -- marked as unreachable (section 7.2.3)
  Expr.EBinary lexpr op rexpr ->
    do
      left <- evaluate io ex st ret lexpr
      right <- evaluate io ex st ret rexpr
      let commonIfNumber operation =
            case (left, right) of
              (Expr.LNumber !l, Expr.LNumber !r) -> pure $ l `operation` r
              _ -> Exception.throw ex $ Error.RuntimeError op.tline "Operands must be numbers." -- TODO
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
              Exception.throw ex $
                Error.RuntimeError op.tline "Operands must be two numbers or two strings."
        TokenType.SLASH -> Expr.LNumber <$> commonIfNumber (/)
        TokenType.STAR -> Expr.LNumber <$> commonIfNumber (*)
        _ -> pure Expr.LNil -- marked as unreachable (section 7.2.5)
  Expr.ECall callee parenLine argumentsExprs isTailCall -> do
    calleeVal <- evaluate io ex st ret callee
    argVals <- traverse (evaluate io ex st ret) argumentsExprs
    case calleeVal of
      Expr.LCallable c -> do
        (callable_arity, callable_call) <- case c of
          Expr.CClass ref ->
            effIO io (readIORef ref)
              <&> (\cv -> (cv.class_arity, Expr.class_call cv io ex st ret ref argVals))
          Expr.CNativeFunction f -> pure $ (f.ln_fun_arity, Expr.ln_fun_call f io ex st argVals)
          Expr.CFunction f -> pure (V.length f.fun_params, call io ex st ret f isTailCall argVals)
        if (callable_arity /= V.length argumentsExprs)
          then
            Exception.throw ex $
              Error.RuntimeError
                parenLine
                ( "Expected "
                    <> BS.pack (show callable_arity)
                    <> " arguments but got "
                    <> BS.pack (show $ V.length argumentsExprs)
                    <> "."
                )
          else callable_call
      _ ->
        Exception.throw ex $
          Error.RuntimeError parenLine "Can only call functions and classes."
  Expr.EGet object fieldName -> do
    evaluate io ex st ret object >>= \case
      Expr.LInstance fields methods -> getInstanceFieldOrMethod io ex fieldName fields methods
      _ ->
        Exception.throw ex $
          Error.RuntimeError fieldName.tline "Only instances have properties."
  Expr.EVariable name distance -> lookUpVariable io ex st name distance
  Expr.EAssign name exprValue accessInfo -> do
    value <- evaluate io ex st ret exprValue
    if accessInfo.distance == (-1)
      then
        State.get st >>= \v -> assignFromMap io ex name accessInfo value (v.globals)
      else State.get st >>= \v -> assignAt io ex accessInfo name value (v.environment)
    pure value
  where
    -- See note for Double comparison in Lox in Expr.hs
    isEqual = (==)

getInstanceFieldOrMethod ::
  (io :> es, ex :> es) =>
  IOE io ->
  Exception Error.RuntimeException ex ->
  TokenType.Token ->
  IORef (Map ShortByteString Expr.LiteralValue) ->
  IORef Expr.LoxRuntimeClass ->
  Eff es Expr.LiteralValue
getInstanceFieldOrMethod io ex fieldName fieldsRef classSuperclass = do
  fields <- effIO io $ readIORef fieldsRef
  case fields M.!? fieldName.lexeme of
    Just v -> pure v
    Nothing -> do
      checkMethodChain io fieldName.lexeme classSuperclass >>= \case
        Just foundMthd -> do
          Expr.LCallable . Expr.CFunction . fst
            <$> bind io fieldsRef classSuperclass foundMthd
        -- TODO fix for when
        Nothing ->
          Exception.throw ex $
            Error.RuntimeError
              fieldName.tline
              ("Undefined property '" <> SBS.fromShort fieldName.lexeme <> "'.")

bind ::
  forall io es.
  (io :> es) =>
  IOE io ->
  IORef (Map ShortByteString Expr.LiteralValue) ->
  IORef Expr.LoxRuntimeClass ->
  Expr.LoxRuntimeFunction ->
  Eff es (Expr.LoxRuntimeFunction, Expr.LiteralValue)
bind io fieldsRef classOfInstance runtimeFun = do
  newClosureEnv <-
    effIO io (newIORef IM.empty)
      <&> \mref -> LocalEnvironment mref runtimeFun.fun_closure
  let classInstance = Expr.LInstance fieldsRef classOfInstance
  define io thisHash classInstance newClosureEnv
  pure (runtimeFun {Expr.fun_closure = newClosureEnv}, classInstance) -- TODO

setInstanceField ::
  (io :> es) =>
  IOE io ->
  TokenType.Token ->
  Expr.LiteralValue ->
  IORef (Map ShortByteString Expr.LiteralValue) ->
  Eff es ()
setInstanceField io name value mref =
  effIO io . modifyIORef' mref $
    \fieldMap -> M.insert name.lexeme value fieldMap

isTruthy :: Expr.LiteralValue -> Bool
isTruthy = \case
  Expr.LNil -> False
  Expr.LBool v -> v
  _ -> True

stringify ::
  (io :> es) =>
  IOE io ->
  Expr.LiteralValue ->
  Eff es ByteString
stringify io = \case
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
  Expr.LString v -> pure $ SBS.fromShort v
  Expr.LCallable (Expr.CClass cref) -> effIO io (readIORef cref) <&> (.class_name) <&> SBS.fromShort
  Expr.LCallable (Expr.CFunction f) -> pure $ SBS.fromShort f.fun_toString
  Expr.LCallable (Expr.CNativeFunction _f) -> pure "<native fn>"
  Expr.LInstance _ cref -> do
    cname <- effIO io (readIORef cref) <&> (.class_name) <&> SBS.fromShort
    pure $ cname <> " instance"

execute ::
  forall es io ex st ret.
  (io :> es, ex :> es, st :> es, ret :> es) =>
  IOE io ->
  Exception Error.RuntimeException ex ->
  State InterpreterState st ->
  EarlyReturn Expr.LiteralValue ret ->
  Stmt.Stmt2 ->
  Eff es ()
execute io ex st ret = \case
  Stmt.SExpression expression -> void $ evaluate io ex st ret expression
  Stmt.SIf condition thenBranch elseBranch -> do
    c <- isTruthy <$> evaluate io ex st ret condition
    if c
      then execute io ex st ret thenBranch
      else traverse_ (execute io ex st ret) elseBranch
  Stmt.SPrint expression ->
    evaluate io ex st ret expression >>= stringify io >>= effIO io . BS.putStrLn
  Stmt.SReturn _ valueExpr -> do
    value <- fromMaybe Expr.LNil <$> traverse (evaluate io ex st ret) valueExpr
    EarlyReturn.returnEarly ret value
  Stmt.SVar accessInfo initializer -> do
    value <- case initializer of
      Nothing -> pure Expr.LNil
      Just v -> evaluate io ex st ret v
    State.get st >>= \v -> define io accessInfo.index value v.environment
  Stmt.SWhile condition body -> do
    whileM
      (isTruthy <$> evaluate io ex st ret condition)
      (execute io ex st ret body)
  Stmt.SBlock statements -> do
    newEnvValueMapRef <- effIO io $ newIORef mempty
    prevEnv <- State.get st <&> (.environment)
    executeBlock
      io
      ex
      st
      ret
      statements
      prevEnv
      (LocalEnvironment newEnvValueMapRef prevEnv)
  Stmt.SClass klassName accessInfo maySuperClassInfo _methods -> do
    origEnv <- State.get st <&> (.environment)
    define io accessInfo.index Expr.LNil origEnv
    maySuperClass <- case maySuperClassInfo of
      Nothing -> pure Nothing
      Just (superclassTok, dist) -> do
        evaluate io ex st ret (Expr.EVariable superclassTok dist)
          >>= \case
            c@(Expr.LCallable (Expr.CClass superclassRef)) -> do
              oldEnv <- State.get st <&> (.environment)
              newEnv <- do
                superEnvRef <- effIO io $ newIORef IM.empty
                pure $ LocalEnvironment superEnvRef oldEnv
              State.modify st $ \s -> s {environment = newEnv}
              define io superHash c newEnv
              pure $ Just superclassRef
            _ ->
              Exception.throw ex $
                Error.RuntimeError superclassTok.tline "Superclass must be a class."
    (mayInitMethdThisClass, thisClassMethods) <-
      V.foldM
        ( \(prevMayInit, acc) next@(Stmt.FFunctionH fname _ _) -> do
            let isInit = fname.lexeme == "init"
            fun <- newFun st next isInit
            let mayInit = if isInit then Just fun else prevMayInit
            pure (mayInit, M.insert fname.lexeme fun acc)
        )
        (Nothing, M.empty)
        _methods
        >>= traverse (effIO io . newIORef)
    mayInitWSuper <-
      case mayInitMethdThisClass of
        Nothing -> do
          case maySuperClass of
            Nothing -> pure Nothing
            Just superClass ->
              checkMethodChain io "init" superClass >>= \case
                Just superInitMthd -> pure $ Just superInitMthd
                Nothing -> pure Nothing
        just -> do pure just
    when (isJust maySuperClass) $ State.modify st $ \s -> s {environment = s.environment._enclosing} -- restore "super" binding
    let (classArity, initMaker) =
          let helper :: -- for type inference
                forall ess ioo exx stt rett.
                ( Int,
                  (ioo :> ess, exx :> ess, stt :> ess, rett :> ess) =>
                  IOE ioo ->
                  Exception Error.RuntimeException exx ->
                  State InterpreterState stt ->
                  EarlyReturn Expr.LiteralValue rett ->
                  IORef (Map ShortByteString Expr.LiteralValue) ->
                  IORef Expr.LoxRuntimeClass ->
                  Vector Expr.LiteralValue ->
                  Eff ess Expr.LiteralValue
                )
              helper = case mayInitWSuper of
                Just originalInitMthd ->
                  let initM io' ex' st' ret' fieldMapRef thisClassRef args = do
                        (initFunc, classInstance) <-
                          bind io' fieldMapRef thisClassRef originalInitMthd
                        _ <- call io' ex' st' ret' initFunc False args
                        pure classInstance
                   in (V.length originalInitMthd.fun_params, initM)
                Nothing ->
                  let initM _io _ex _st _ret fieldMapRef thisClassRef _args =
                        pure $ Expr.LInstance fieldMapRef thisClassRef
                   in (0, initM)
           in helper
    klass <-
      effIO io $
        Expr.LCallable . Expr.CClass
          <$> ( newIORef $
                  Expr.LRClass
                    { Expr.class_name = klassName.lexeme,
                      Expr.class_arity = classArity,
                      Expr.class_call = \io' ex' st' ret' thisClassRef args -> do
                        fieldMapRef <- effIO io' $ newIORef M.empty
                        initMaker io' ex' st' ret' fieldMapRef thisClassRef args,
                      Expr.class_methods = thisClassMethods,
                      Expr.class_superclass = maySuperClass
                    }
              )

    assignFromMap io ex klassName accessInfo klass origEnv.values
  Stmt.SFunction accessInfo f -> do
    -- environment of the function where it is declared
    function <- Expr.LCallable . Expr.CFunction <$> newFun st f False
    State.get st >>= \s -> define io accessInfo.index function s.environment

newFun ::
  (st :> es) =>
  State InterpreterState st ->
  Stmt.FunctionH2 ->
  Bool ->
  Eff es Expr.LoxRuntimeFunction
newFun st fun isInitializer = case fun of
  Stmt.FFunctionH name params body -> do
    -- environment of the function where it is declared
    closure <- State.get st <&> (.environment)
    let function =
          Expr.LRFunction
            { Expr.fun_toString = "<fn " <> name.lexeme <> ">",
              Expr.fun_token_line = name.tline,
              Expr.fun_params = params,
              Expr.fun_closure = closure,
              Expr.fun_isInitializer = isInitializer,
              Expr.fun_body = body
            }
    pure function

call ::
  forall es io ex st ret.
  (io :> es, ex :> es, st :> es, ret :> es) =>
  IOE io ->
  Exception Error.RuntimeException ex ->
  State InterpreterState st ->
  EarlyReturn Expr.LiteralValue ret ->
  Expr.LoxRuntimeFunction ->
  Bool ->
  Vector Expr.LiteralValue ->
  Eff es Expr.LiteralValue
call io ex st ret f isTailCall args = do
  -- environment of the function before being called
  prevEnv <- State.get st <&> (.environment)
  if isTailCall
    then do
      effIO io $ modifyIORef' prevEnv.values $ \prevValueMap ->
        f.fun_params
          & V.ifoldl'
            -- unsafe index is safe, since `call` is not ran if the fun_arity does not match with the arguments
            (\vm ixvec ix -> IM.insert ix.index (args `V.unsafeIndex` ixvec) vm)
            prevValueMap
      runFun io ex st ret
    else do
      let newValueMap =
            V.zipWith (\accessInfo -> (accessInfo.index,)) f.fun_params args
              & V.toList
              & IM.fromList
      funcEnvironment <-
        effIO io $ newIORef newValueMap <&> (`LocalEnvironment` f.fun_closure)
      executeWithinEnvs ex st (\e -> within io e st) prevEnv funcEnvironment
  where
    -- if the function does not return via a return stmt, it will default to nil
    runFun io' ex' st' ret' = executeStmts io' ex' st' ret' f.fun_body
    within io' ex' st' = EarlyReturn.withEarlyReturn $ \ret' ->
      runFun io' ex' st' ret' >>= \v ->
        if f.fun_isInitializer
          then execIfInit io' ex'
          else pure v
    execIfInit io' ex' = do
      effIO io' (readIORef f.fun_closure.values) >>= \vmap ->
        case vmap IM.!? thisHash of
          Just thisInstance ->
            pure thisInstance
          Nothing ->
            Exception.throw ex' $
              Error.RuntimeError
                f.fun_token_line
                "Error in interpreter, should have found reference to 'this' to return in _.init() class method."

executeBlock ::
  (io :> es, ex :> es, st :> es, ret :> es) =>
  IOE io ->
  Exception Error.RuntimeException ex ->
  State InterpreterState st ->
  EarlyReturn Expr.LiteralValue ret ->
  Vector Stmt.Stmt2 ->
  Environment ->
  Environment ->
  Eff es ()
executeBlock io ex st ret statements =
  executeWithinEnvs ex st (\e -> executeStmts io e st ret statements >> pure ())

executeWithinEnvs ::
  (ex :> es, st :> es) =>
  Exception Error.RuntimeException ex ->
  State InterpreterState st ->
  (forall e. Exception Error.RuntimeException e -> Eff (e :& es) a) ->
  Environment ->
  Environment ->
  Eff es a
executeWithinEnvs ex st action prevEnv tempEnv = do
  State.modify st $ \s ->
    s {environment = tempEnv}
  finallyE ex action $ do
    State.modify st $ \s -> s {environment = prevEnv}
  where
    finallyE ::
      (ex :> es) =>
      Exception exn ex ->
      (forall e. Exception exn e -> Eff (e :& es) a) ->
      Eff es b ->
      Eff es a
    finallyE e first closer = do
      r <- Exception.try first
      closer
      either (Exception.throw e) (pure) r

interpret ::
  (io :> es) => IOE io -> Vector Stmt.Stmt2 -> Eff es Error.ErrorPresent
interpret io statements = do
  initialInterpreterState <- effIO io $ do
    globalsRef <- newIORef globals
    pure $
      InterpreterState
        { environment = GlobalEnvironment globalsRef,
          globals = globalsRef
        }
  value :: Either Error.RuntimeException Expr.LiteralValue <-
    State.evalState initialInterpreterState $ \st -> Exception.try $ \ex -> EarlyReturn.withEarlyReturn $ \ret -> executeStmts io ex st ret statements
  case value of
    Left e -> Error.reportRuntimeError io e >> pure Error.Error
    Right _value -> pure Error.NoError
  where
    globals :: ValueMap
    globals =
      IM.fromList $
        (\(k, v) -> (Hashable.hash @ShortByteString k, v))
          <$> [ ( "clock",
                  Expr.LCallable $
                    Expr.CNativeFunction $
                      Expr.LNFunction
                        { Expr.ln_fun_arity = 0,
                          Expr.ln_fun_call = \io' _ex' _st' _args ->
                            -- realToFrac & fromIntegral treat NominalDiffTime as seconds
                            Expr.LNumber . realToFrac <$> effIO io' Time.getPOSIXTime
                        }
                ),
                ( "read",
                  Expr.LCallable $
                    Expr.CNativeFunction $
                      Expr.LNFunction
                        { Expr.ln_fun_arity = 0,
                          Expr.ln_fun_call = \io' _ex' _st' _args ->
                            effIO io' $ readByte stdin
                        }
                ),
                ( "utf",
                  -- see https://docs.oracle.com/javase/specs/jls/se20/html/jls-5.html#jls-5.1.3 and original java loxlox
                  Expr.LCallable $
                    Expr.CNativeFunction $
                      Expr.LNFunction
                        { Expr.ln_fun_arity = 4,
                          Expr.ln_fun_call = \_io _ex _st _args -> do
                            let bytes =
                                  V.foldr'
                                    (\n acc -> case n of Expr.LNumber d -> round d : acc; _else -> acc)
                                    []
                                    _args
                            pure $ Expr.LString $ SBS.pack bytes
                        }
                ),
                ( "exit",
                  Expr.LCallable $
                    Expr.CNativeFunction $
                      Expr.LNFunction
                        { Expr.ln_fun_arity = 1,
                          Expr.ln_fun_call = \_io _ex _st _args ->
                            let exitCode = case _args V.! 0 of
                                  Expr.LNumber n -> floor n
                                  _ -> 70
                             in effIO _io $ System.Exit.exitWith $ System.Exit.ExitFailure exitCode
                        }
                ),
                ( "printerr",
                  Expr.LCallable $
                    Expr.CNativeFunction $
                      Expr.LNFunction
                        { Expr.ln_fun_arity = 1,
                          Expr.ln_fun_call = \io' _ex _st _args -> do
                            errstr <- stringify io' $ _args V.! 0
                            effIO io' $ BS.hPutStrLn stderr errstr
                            pure Expr.LNil
                        }
                )
              ]

-- for loxlox: https://github.com/mrjameshamilton/loxlox/tree/main/bin
-- see https://docs.oracle.com/en/java/javase/20/docs/api/java.base/java/io/InputStream.html#read()
readByte :: Handle -> IO Expr.LiteralValue
readByte handle = do
  buffer :: Ptr Word8 <- mallocBytes 1
  actReadBytes <- hGetBuf handle buffer 1
  res <-
    if actReadBytes == 0
      then pure Expr.LNil
      else do
        byte <- peek buffer
        pure $ Expr.LNumber $ fromIntegral byte
  free buffer
  pure res

executeStmts ::
  (io :> es, ex :> es, st :> es, ret :> es) =>
  IOE io ->
  Exception Error.RuntimeException ex ->
  State InterpreterState st ->
  EarlyReturn Expr.LiteralValue ret ->
  Vector Stmt.Stmt2 ->
  Eff es Expr.LiteralValue
executeStmts io ex st ret v = traverse_ (execute io ex st ret) v >> pure Expr.LNil

recPrintEnvs ::
  (io :> es) =>
  IOE io ->
  Environment ->
  Int ->
  Eff es ()
recPrintEnvs io env count = case env of
  GlobalEnvironment v -> printEnvRef v
  LocalEnvironment v enc -> printEnvRef v >> recPrintEnvs io enc (count + 1)
  where
    printEnvRef ref = do
      mapv <- effIO io (readIORef ref)
      stringMap <- traverse (stringify io) mapv
      effIO io . putStrLn $ show stringMap <> " " <> show count

checkMethodChain ::
  (io :> es) =>
  IOE io ->
  ShortByteString ->
  IORef Expr.LoxRuntimeClass ->
  Eff es (Maybe Expr.LoxRuntimeFunction)
checkMethodChain io fieldName = go
  where
    go ref = do
      klass <- effIO io (readIORef ref)
      checkMethods klass >>= \case
        Just fun -> pure $ Just fun
        Nothing -> case klass.class_superclass of
          Nothing -> pure Nothing
          Just supref -> go supref
      where
        checkMethods klass = do
          vmap <- effIO io (readIORef klass.class_methods)
          pure (vmap M.!? fieldName)

thisHash :: Int
thisHash = Hashable.hash ("this" :: ShortByteString)

superHash :: Int
superHash = Hashable.hash ("super" :: ShortByteString)
