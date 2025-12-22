{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Resolver (runResolver) where

import Bluefin.Eff
import Bluefin.IO (IOE)
import Bluefin.State (State, evalState)
import Bluefin.State qualified as State
import Bluefin.Writer (Writer)
import Control.Monad (when)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Vector (Vector)
import Error qualified
import Expr qualified
import Stmt qualified
import TokenType qualified

data FunctionType
  = FTNone
  | FTFunction
  | FTInitializer
  | FTMethod
  deriving (Eq)

data ClassType
  = CTNone
  | CTClass
  | CTSubclass

data ResolverState = ResolverState
  { scopes :: ![Map ShortByteString Bool]
  , currentFunction :: !FunctionType
  , currentClass :: !ClassType
  }

-- type ResolverM a = RWST () Error.ErrorPresent ResolverState IO a

runResolver
  :: (io :> es, w :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Vector Stmt.Stmt1
  -> Eff es (Vector Stmt.Stmt2)
runResolver io w stmts =
  evalState initialResolverState $ \st -> resolveStmts st stmts
  where
    initialResolverState = ResolverState {scopes = [], currentFunction = FTNone, currentClass = CTNone}
    resolveStmts st = traverse (resolveStmt io w st)

resolveStmt
  :: (io :> es, w :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> State ResolverState st
  -> Stmt.Stmt1
  -> Eff es Stmt.Stmt2
resolveStmt io w st = \case
  Stmt.SBlock stmts -> do
    beginScope st
    newStmts <- traverse (resolveStmt io w st) stmts
    endScope st
    pure $ Stmt.SBlock newStmts
  Stmt.SClass name superclass _methods -> do
    enclosingClass <- State.get st <&> (.currentClass)
    State.modify st $ \s -> s {currentClass = CTClass}
    declare io w st name
    define st name
    nSuperclass <- case superclass of
      Just (superclassTok, ()) -> do
        when (superclassTok.lexeme == name.lexeme) $
          Error.resolverError io w superclassTok "A class can't inherit from itself."
        State.modify st $ \s -> s {currentClass = CTSubclass} -- TODO
        nSuperClass <-
          Just . (superclassTok,) <$> resolveVariableName io w st superclassTok
        beginScope st -- start scope for "super"
        define st (TokenType.Token TokenType.SUPER "super" 0) -- TODO remove?
        pure nSuperClass
      Nothing -> pure Nothing

    beginScope st
    newSt <-
      State.get st >>= \oldSt -> do
        newSc <- case oldSt.scopes of
          [] -> do
            Error.resolverError io w name "Failure in resolver, bug in interpreter"
            pure []
          (x : xs) -> pure $ M.insert "this" True x : xs
        pure $ oldSt {scopes = newSc}
    State.put st newSt

    nMethods <-
      traverse
        ( \(Stmt.FFunctionH fname params body) -> do
            let funtype = if fname.lexeme == "init" then FTInitializer else FTMethod
            nBody <- resolveFunction io w st params body funtype
            pure $ Stmt.FFunctionH fname params nBody
        )
        _methods
    endScope st

    when (isJust nSuperclass) (endScope st) -- end scope for "super"
    State.modify st $ \s -> s {currentClass = enclosingClass}
    pure $ Stmt.SClass name nSuperclass nMethods
  Stmt.SVar name initializer -> do
    declare io w st name
    nInitializer <- traverse (resolveExpr io w st) initializer
    define st name
    pure $ Stmt.SVar name nInitializer
  Stmt.SFunction (Stmt.FFunctionH name params body) -> do
    declare io w st name >> define st name
    nBody <- resolveFunction io w st params body FTFunction
    pure $ Stmt.SFunction $ Stmt.FFunctionH name params nBody
  Stmt.SExpression expr -> Stmt.SExpression <$> resolveExpr io w st expr
  Stmt.SIf cond thenBody elseBody -> do
    nCond <- resolveExpr io w st cond
    nThen <- resolveStmt io w st thenBody
    nElse <- traverse (resolveStmt io w st) elseBody
    pure $ Stmt.SIf nCond nThen nElse
  Stmt.SPrint expr -> Stmt.SPrint <$> resolveExpr io w st expr
  Stmt.SReturn t expr -> do
    curF <- State.get st <&> (.currentFunction)
    when (curF == FTNone) $
      Error.resolverError io w t "Can't return from top-level code."
    Stmt.SReturn t
      <$> traverse
        ( \e -> do
            when (curF == FTInitializer) $
              Error.resolverError io w t "Can't return a value from an initializer."
            resolveExpr io w st e
        )
        expr
  Stmt.SWhile cond body -> do
    nCond <- resolveExpr io w st cond
    nBody <- resolveStmt io w st body
    pure $ Stmt.SWhile nCond nBody

resolveFunction
  :: (io :> es, w :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> State ResolverState st
  -> Vector TokenType.Token
  -> Vector Stmt.Stmt1
  -> FunctionType
  -> Eff es (Vector Stmt.Stmt2)
resolveFunction io w st params body funType = do
  enclosing <- State.get st <&> (.currentFunction)
  State.modify st $ \s -> s {currentFunction = funType}
  beginScope st
  traverse_ (\param -> declare io w st param >> define st param) params
  nBody <- traverse (resolveStmt io w st) body
  State.modify st $ \s -> s {currentFunction = enclosing}
  endScope st
  pure nBody

resolveVariableName
  :: (io :> es, w :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> State ResolverState st
  -> TokenType.Token
  -> Eff es Int
resolveVariableName io w st name = do
  State.get st <&> (.scopes) >>= \case
    [] -> pure ()
    (closestScope : _) ->
      case closestScope M.!? name.lexeme of
        Just False ->
          Error.resolverError
            io
            w
            name
            "Can't read local variable in its own initializer."
        _ -> pure ()
  resolveLocal st name.lexeme

resolveExpr
  :: (io :> es, w :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> State ResolverState st
  -> Expr.Expr1
  -> Eff es Expr.Expr2
resolveExpr io w st = \case
  Expr.EVariable name _ -> do
    distance <- resolveVariableName io w st name
    pure $ Expr.EVariable name distance
  Expr.EAssign name value _ -> do
    nValue <- resolveExpr io w st value
    distance <- resolveLocal st name.lexeme
    pure $ Expr.EAssign name nValue distance
  Expr.EBinary left t right -> do
    nL <- resolveExpr io w st left
    nR <- resolveExpr io w st right
    pure $ Expr.EBinary nL t nR
  Expr.ECall callee t args -> do
    nCallee <- resolveExpr io w st callee
    nArgs <- traverse (resolveExpr io w st) args
    pure $ Expr.ECall nCallee t nArgs
  Expr.EGet object name -> do
    nObject <- resolveExpr io w st object
    pure $ Expr.EGet nObject name
  Expr.EGrouping expr -> Expr.EGrouping <$> resolveExpr io w st expr
  Expr.ELiteral l -> pure $ Expr.ELiteral l
  Expr.ELogical left t right -> do
    nL <- resolveExpr io w st left
    nR <- resolveExpr io w st right
    pure $ Expr.ELogical nL t nR
  Expr.ESet object name value -> do
    nValue <- resolveExpr io w st value
    nObject <- resolveExpr io w st object
    pure $ Expr.ESet nObject name nValue
  Expr.ESuper keyword () mthd -> do
    State.get st <&> (.currentClass) >>= \case
      CTNone ->
        Error.resolverError io w keyword "Can't use 'super' outside of a class."
      CTClass ->
        Error.resolverError
          io
          w
          keyword
          "Can't use 'super' in a class with no superclass."
      CTSubclass -> pure ()
    kDist <- resolveLocal st keyword.lexeme
    pure $ Expr.ESuper keyword kDist mthd -- TODO
  Expr.EThis keyword _ -> do
    distance <-
      State.get st <&> (.currentClass) >>= \case
        CTNone -> do
          Error.resolverError io w keyword "Can't use 'this' outside of a class."
          pure 0
        _ -> resolveLocal st keyword.lexeme
    pure $ Expr.EThis keyword distance
  Expr.EUnary t expr -> Expr.EUnary t <$> resolveExpr io w st expr

beginScope
  :: st :> es
  => State ResolverState st
  -> Eff es ()
beginScope st = State.modify st $ \s -> s {scopes = M.empty : s.scopes}

endScope :: st :> es => State ResolverState st -> Eff es ()
endScope st = State.modify st $ \s ->
  let ns = case s.scopes of
        [] -> []
        (_ : xs) -> xs
   in s {scopes = ns}

declare
  :: (io :> es, w :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> State ResolverState st
  -> TokenType.Token
  -> Eff es ()
declare io w st name =
  State.get st >>= \s ->
    case s.scopes of
      [] -> pure ()
      (oldM : xs) ->
        --  M.insert name.lexeme True x : xs
        case M.insertLookupWithKey (\_k _o n -> n) name.lexeme False oldM of
          (Just _old, _) ->
            Error.resolverError io w name "Already a variable with this name in this scope."
          (Nothing, newM) -> State.put st $ s {scopes = newM : xs}

define :: st :> es => State ResolverState st -> TokenType.Token -> Eff es ()
define st name =
  State.modify st $ \s ->
    let newSc =
          case s.scopes of
            [] -> []
            (x : xs) -> M.insert name.lexeme True x : xs
     in s {scopes = newSc}

resolveLocal
  :: st :> es => State ResolverState st -> ShortByteString -> Eff es Int
resolveLocal st name = do
  scopes <- State.get st <&> (.scopes)
  go 0 scopes
  where
    go :: Int -> [Map ShortByteString Bool] -> Eff es Int
    go count = \case
      [] -> pure (-1)
      (x : xs) ->
        if M.member name x
          then pure count
          else go (count + 1) xs
