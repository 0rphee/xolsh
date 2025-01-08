{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Resolver (runResolver) where

import Control.Monad (when)
import Control.Monad.RWS.CPS (RWST, evalRWST)
import Control.Monad.State.Class qualified as State
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Vector (Vector)
import Error qualified
import Expr qualified
import Stmt qualified
import TokenType qualified

data FunctionType
  = None
  | Function
  deriving (Eq)

data ResolverState = ResolverState
  { scopes :: ![Map ByteString Bool]
  , currentFunction :: !FunctionType
  }

type ResolverM a = RWST () Error.ErrorPresent ResolverState IO a

runResolver
  :: Vector Stmt.Stmt1 -> IO (Maybe (Vector Stmt.Stmt2))
runResolver stmts = do
  (r, w) <- evalRWST (resolveStmts stmts) () initialResolverState
  pure $ case w of
    Error.NoError -> Just r
    Error.Error -> Nothing
  where
    initialResolverState = ResolverState {scopes = [], currentFunction = None}
    resolveStmts = traverse resolveStmt

resolveStmt :: Stmt.Stmt1 -> ResolverM Stmt.Stmt2
resolveStmt = \case
  Stmt.SBlock stmts -> do
    beginScope
    newStmts <- traverse resolveStmt stmts
    endScope
    pure $ Stmt.SBlock newStmts
  Stmt.SVar name initializer -> do
    declare name
    nInitializer <- traverse resolveExpr initializer
    define name
    pure $ Stmt.SVar name nInitializer
  Stmt.SFunction name params body -> do
    declare name >> define name
    nBody <- resolveFunction params body Function
    pure $ Stmt.SFunction name params nBody
  Stmt.SExpression expr -> Stmt.SExpression <$> resolveExpr expr
  Stmt.SIf cond thenBody elseBody -> do
    nCond <- resolveExpr cond
    nThen <- resolveStmt thenBody
    nElse <- traverse resolveStmt elseBody
    pure $ Stmt.SIf nCond nThen nElse
  Stmt.SPrint expr -> Stmt.SPrint <$> resolveExpr expr
  Stmt.SReturn t expr -> do
    curF <- State.gets (.currentFunction)
    when (curF == None) $ Error.resolverError t "Can't return from top-level code."
    Stmt.SReturn t <$> traverse resolveExpr expr
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
  beginScope
  traverse_ (\param -> declare param >> define param) params
  nBody <- traverse resolveStmt body
  State.modify' $ \st -> st {currentFunction = enclosing}
  endScope
  pure nBody

resolveExpr :: Expr.Expr1 -> ResolverM Expr.Expr2
resolveExpr = \case
  (Expr.EVariable name _) -> do
    State.gets (.scopes) >>= \case
      [] -> pure ()
      (closestScope : _) ->
        case closestScope M.!? name.lexeme of
          Just v
            | not v ->
                Error.resolverError name "Can't read local variable in its own initializer."
          _ -> pure ()
    distance <- resolveLocal name.lexeme
    pure $ Expr.EVariable name distance
  (Expr.EAssign name value _) -> do
    nValue <- resolveExpr value
    distance <- resolveLocal name.lexeme
    pure $ Expr.EAssign name nValue distance
  Expr.EBinary left t right -> do
    nL <- resolveExpr left
    nR <- resolveExpr right
    pure $ Expr.EBinary nL t nR
  Expr.ECall callee t args -> do
    nCallee <- resolveExpr callee
    nArgs <- traverse resolveExpr args
    pure $ Expr.ECall nCallee t nArgs
  Expr.EGrouping expr -> Expr.EGrouping <$> resolveExpr expr
  Expr.ELiteral l -> pure $ Expr.ELiteral l
  Expr.ELogical left t right -> do
    nL <- resolveExpr left
    nR <- resolveExpr right
    pure $ Expr.ELogical nL t nR
  Expr.EUnary t expr -> Expr.EUnary t <$> resolveExpr expr

beginScope :: ResolverM ()
beginScope = State.modify' $ \st -> st {scopes = M.empty : st.scopes}

endScope :: ResolverM ()
endScope = State.modify $ \st ->
  let ns = case st.scopes of
        [] -> []
        (_ : xs) -> xs
   in st {scopes = ns}

declare :: TokenType.Token -> ResolverM ()
declare name =
  State.get >>= \st ->
    case st.scopes of
      [] -> pure ()
      (oldM : xs) ->
        --  M.insert name.lexeme True x : xs
        case M.insertLookupWithKey (\_k _o n -> n) name.lexeme False oldM of
          (Just _old, _) -> Error.resolverError name "Already a variable with this name in this scope."
          (Nothing, newM) -> State.put $ st {scopes = newM : xs}

define :: TokenType.Token -> ResolverM ()
define name =
  State.modify' $ \st ->
    let newSc =
          case st.scopes of
            [] -> []
            (x : xs) -> M.insert name.lexeme True x : xs
     in st {scopes = newSc}

resolveLocal :: ByteString -> ResolverM Int
resolveLocal name = do
  scopes <- State.gets (.scopes)
  go 0 scopes
  where
    go :: Int -> [Map ByteString Bool] -> ResolverM Int
    go count = \case
      [] -> pure (-1)
      (x : xs) ->
        if M.member name x
          then pure count
          else go (count + 1) xs
