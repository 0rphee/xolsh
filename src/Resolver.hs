{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Resolver (runResolver) where

import Control.Monad (when)
import Control.Monad.RWS.CPS qualified as RWST
import Control.Monad.State.Class qualified as State
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (traverse_)
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
  { globalScope :: !(Map ShortByteString (Bool, Int))
  , scopes :: ![Map ShortByteString (Bool, Int)]
  , currentFunction :: !FunctionType
  , currentClass :: !ClassType
  }

type ResolverM a = RWST.RWST () Error.ErrorPresent ResolverState IO a

runResolver
  :: Vector Stmt.Stmt1 -> IO (Maybe (Vector Stmt.Stmt2))
runResolver stmts = do
  (r, w) <- RWST.evalRWST (resolveStmts stmts) () initialResolverState
  pure $ case w of
    Error.NoError -> Just r
    Error.Error -> Nothing
  where
    initialResolverState =
      ResolverState
        { globalScope = M.empty -- Todo, add builtin values
        , scopes = []
        , currentFunction = FTNone
        , currentClass = CTNone
        }
    resolveStmts = traverse resolveStmt

resolveStmt :: Stmt.Stmt1 -> ResolverM Stmt.Stmt2
resolveStmt = \case
  Stmt.SBlock stmts -> do
    beginScope
    newStmts <- traverse resolveStmt stmts
    endScope
    pure $ Stmt.SBlock newStmts
  Stmt.SClass name superclass _methods -> do
    enclosingClass <- State.gets (.currentClass)
    State.modify' $ \st -> st {currentClass = CTClass}
    classNameInfo <- declare name
    define name
    nSuperclass <- case superclass of
      Just (superclassTok, ()) -> do
        when (superclassTok.lexeme == name.lexeme) $
          Error.resolverError superclassTok "A class can't inherit from itself."
        State.modify' $ \st -> st {currentClass = CTSubclass} -- TODO
        nSuperClass <- Just . (superclassTok,) <$> resolveVariableName superclassTok
        beginScope -- start scope for "super"
        define (TokenType.Token TokenType.SUPER "super" 0) -- TODO remove?
        pure nSuperClass
      Nothing -> pure Nothing

    nMethods <- withinNewScope $ do
      newSt <-
        State.get >>= \oldSt -> do
          newSc <- case oldSt.scopes of
            [] -> do
              Error.resolverError name "Failure in resolver, bug in interpreter"
              pure []
            (x : xs) -> do
              pure $ M.insert "this" (True, M.size x + 1) x : xs
          pure $ oldSt {scopes = newSc}
      State.put newSt
      traverse
        ( \(Stmt.FFunctionH fname () params body) -> do
            let funtype = if fname.lexeme == "init" then FTInitializer else FTMethod
            -- TODO maybe remove declare???
            nameInfo <- declare fname
            nBody <- resolveFunction params body funtype
            pure $ Stmt.FFunctionH fname nameInfo params nBody
        )
        _methods

    when (isJust nSuperclass) endScope -- end scope for "super"
    State.modify' $ \st -> st {currentClass = enclosingClass}
    pure $ Stmt.SClass classNameInfo nSuperclass nMethods
  Stmt.SVar name initializer -> do
    nameInfo <- declare name
    nInitializer <- traverse resolveExpr initializer
    define name
    pure $ Stmt.SVar nameInfo nInitializer
  Stmt.SFunction (Stmt.FFunctionH name () params body) -> do
    nameInfo <- declare name <* define name
    nBody <- resolveFunction params body FTFunction
    pure $ Stmt.SFunction $ Stmt.FFunctionH name nameInfo params nBody
  Stmt.SExpression expr -> Stmt.SExpression <$> resolveExpr expr
  Stmt.SIf cond thenBody elseBody -> do
    nCond <- resolveExpr cond
    nThen <- resolveStmt thenBody
    nElse <- traverse resolveStmt elseBody
    pure $ Stmt.SIf nCond nThen nElse
  Stmt.SPrint expr -> Stmt.SPrint <$> resolveExpr expr
  Stmt.SReturn t expr -> do
    curF <- State.gets (.currentFunction)
    when (curF == FTNone) $
      Error.resolverError t "Can't return from top-level code."
    Stmt.SReturn t
      <$> traverse
        ( \e -> do
            when (curF == FTInitializer) $
              Error.resolverError t "Can't return a value from an initializer."
            resolveExpr e
        )
        expr
  Stmt.SWhile cond body -> do
    nCond <- resolveExpr cond
    nBody <- resolveStmt body
    pure $ Stmt.SWhile nCond nBody

resolveFunction
  :: Vector TokenType.Token
  -> Vector Stmt.Stmt1
  -> FunctionType
  -> ResolverM (Vector Stmt.Stmt2)
resolveFunction params body funType = do
  enclosing <- State.gets (.currentFunction)
  State.modify' $ \st -> st {currentFunction = funType}
  nBody <- withinNewScope $ do
    traverse_ (\param -> declare param >> define param) params
    nBody <- traverse resolveStmt body
    State.modify' $ \st -> st {currentFunction = enclosing}
    pure nBody
  pure nBody

resolveVariableName :: TokenType.Token -> ResolverM Expr.NameInfo
resolveVariableName name = do
  State.gets (.scopes) >>= \case
    [] -> pure ()
    (closestScope : _) ->
      case closestScope M.!? name.lexeme of
        Just (False, _) ->
          Error.resolverError name "Can't read local variable in its own initializer."
        _ -> pure ()
  resolveLocal name

resolveExpr :: Expr.Expr1 -> ResolverM Expr.Expr2
resolveExpr = \case
  Expr.EVariable name _ -> do
    nameInfo <- resolveVariableName name
    -- State.modify' $ \st -> st {currentBindings = st.currentBindings + 1}
    -- currBindingsIndex <- State.gets (.currentBindings)
    pure $
      Expr.EVariable
        name
        nameInfo
  -- ( Expr.NameInfo
  --     { Expr.nameInfo_index = currBindingsIndex
  --     , Expr.nameInfo_scope = distance
  --     }
  -- )
  Expr.EAssign name value _ -> do
    nValue <- resolveExpr value
    distance <- resolveLocal name
    pure $ Expr.EAssign name nValue distance
  Expr.EBinary left t right -> do
    nL <- resolveExpr left
    nR <- resolveExpr right
    pure $ Expr.EBinary nL t nR
  Expr.ECall callee t args -> do
    nCallee <- resolveExpr callee
    nArgs <- traverse resolveExpr args
    pure $ Expr.ECall nCallee t nArgs
  Expr.EGet object name -> do
    nObject <- resolveExpr object
    pure $ Expr.EGet nObject name
  Expr.EGrouping expr -> Expr.EGrouping <$> resolveExpr expr
  Expr.ELiteral l -> pure $ Expr.ELiteral l
  Expr.ELogical left t right -> do
    nL <- resolveExpr left
    nR <- resolveExpr right
    pure $ Expr.ELogical nL t nR
  Expr.ESet object name value -> do
    nValue <- resolveExpr value
    nObject <- resolveExpr object
    pure $ Expr.ESet nObject name nValue
  Expr.ESuper keyword () mthd -> do
    State.gets (.currentClass) >>= \case
      CTNone ->
        Error.resolverError keyword "Can't use 'super' outside of a class."
      CTClass ->
        Error.resolverError keyword "Can't use 'super' in a class with no superclass."
      CTSubclass -> pure ()
    kDist <- resolveLocal keyword
    pure $ Expr.ESuper keyword kDist mthd -- TODO
  Expr.EThis keyword _ -> do
    distance <-
      State.gets (.currentClass) >>= \case
        CTNone -> do
          Error.resolverError keyword "Can't use 'this' outside of a class."
          pure $ Expr.NameInfo {Expr.nameInfo_index = 0, Expr.nameInfo_scope = 0}
        _ -> resolveLocal keyword
    pure $ Expr.EThis keyword distance
  Expr.EUnary t expr -> Expr.EUnary t <$> resolveExpr expr

beginScope :: ResolverM ()
beginScope = State.modify' $ \st -> st {scopes = M.empty : st.scopes}

endScope :: ResolverM ()
endScope = State.modify $ \st ->
  let ns = case st.scopes of
        [] -> []
        (_ : xs) -> xs
   in st {scopes = ns}

withinNewScope :: ResolverM a -> ResolverM a
withinNewScope act = beginScope >> act <* endScope

declare :: TokenType.Token -> ResolverM Expr.NameInfo
declare name =
  State.get >>= \st ->
    case st.scopes of
      [] -> do
        oldM <- State.gets (.globalScope)
        let ix = M.size oldM + 1
        case M.insertLookupWithKey @_ @(Bool, Int)
          (\_k _o n -> n)
          name.lexeme
          (False, ix)
          oldM of
          (Just _old, _) -> do
            Error.resolverError
              name
              "Already a variable with this name in the global scope."
            pure $ Expr.NameInfo {Expr.nameInfo_index = 0, Expr.nameInfo_scope = 0}
          (Nothing, newM) -> do
            State.put $ st {globalScope = newM}
            pure $ Expr.NameInfo {Expr.nameInfo_index = ix, Expr.nameInfo_scope = -1}
      (oldM : xs) ->
        let ix = M.size oldM + 1
         in case M.insertLookupWithKey @_ @(Bool, Int)
              (\_k _o n -> n)
              name.lexeme
              (False, ix)
              oldM of
              (Just _old, _) -> do
                Error.resolverError name "Already a variable with this name in this scope."
                pure $ Expr.NameInfo {Expr.nameInfo_index = 0, Expr.nameInfo_scope = 0}
              (Nothing, newM) -> do
                State.put $ st {scopes = newM : xs}
                pure $ Expr.NameInfo {Expr.nameInfo_index = ix, Expr.nameInfo_scope = 0}

define :: TokenType.Token -> ResolverM ()
define name =
  State.modify' $ \st -> do
    case st.scopes of
      [] ->
        st
          { globalScope =
              M.insertWith
                (\(_false, n) (_true, _) -> (True, n))
                name.lexeme
                (True, M.size st.globalScope + 1)
                st.globalScope
          }
      (x : xs) ->
        st
          { scopes =
              M.insertWith
                (\(_false, n) (_true, _) -> (True, n))
                name.lexeme
                (True, M.size x + 1)
                x
                : xs
          }

resolveLocal :: TokenType.Token -> ResolverM Expr.NameInfo
resolveLocal name = do
  scopes <- State.gets (.scopes)
  go 0 scopes
  where
    go :: Int -> [Map ShortByteString (Bool, Int)] -> ResolverM Expr.NameInfo
    go distanceCount = \case
      [] -> do
        globalScope <- State.gets (.globalScope)
        case M.lookup name.lexeme globalScope of
          Just (_, ix) -> pure (Expr.NameInfo {Expr.nameInfo_index = ix, Expr.nameInfo_scope = -1})
          Nothing -> do
            Error.resolverError name "Name without referent -1."
            pure (Expr.NameInfo {Expr.nameInfo_index = -1, Expr.nameInfo_scope = -1})
      (x : xs) ->
        case M.lookup name.lexeme x of
          Just (_, ix) ->
            pure
              ( Expr.NameInfo
                  { Expr.nameInfo_index = ix
                  , Expr.nameInfo_scope = distanceCount
                  }
              )
          Nothing -> go (distanceCount + 1) xs

modifyAndGet :: (ResolverState -> ResolverState) -> ResolverM ResolverState
modifyAndGet f = do
  newState <- State.gets f
  State.put newState
  pure newState
