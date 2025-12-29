{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Parser (runParse) where

import Bluefin.Eff (Eff, (:>))
import Bluefin.Exception (Exception)
import Bluefin.Exception qualified as Exception
import Bluefin.IO (IOE)
import Bluefin.State (State)
import Bluefin.State qualified as State
import Bluefin.Writer (Writer)
import Control.Monad (void, when)
import Data.ByteString.Char8 (ByteString)
import Data.Functor ((<&>))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Error qualified
import Expr
import Stmt qualified
import TokenType qualified
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB

data Parser = Parser
  { current :: !Int
  , tokens :: !(Vector TokenType.Token)
  }
  deriving (Show)

data ParseException = ParseException

runParse
  :: (io :> es, w :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Vector TokenType.Token
  -> Eff es (Vector Stmt.Stmt1)
runParse io w tokens = State.evalState initialParserState $ parse io w
  where
    initialParserState = Parser {current = 0, tokens = tokens}

parse
  :: (io :> es, w :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> State Parser st
  -> Eff es (Vector Stmt.Stmt1)
parse io w st = go VB.empty
  where
    go accum = do
      e <- not <$> isAtEnd st
      if e
        then do
          next <- declaration io w st
          case next of
            Just n -> go (accum <> VB.singleton n)
            Nothing -> go accum
        else pure $ VB.build accum

varDeclaration
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
varDeclaration io w ex st = do
  name <- consume io w ex st TokenType.IDENTIFIER "Expect variable name."
  m <- match st [TokenType.EQUAL]
  initializer <- if m then Just <$> expression io w ex st else pure Nothing
  consume io w ex st TokenType.SEMICOLON "Expect ';' after variable declaration."
  pure $ Stmt.SVar name initializer

{-# INLINE declaration #-}
declaration
  :: (io :> es, w :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> State Parser st
  -> Eff es (Maybe Stmt.Stmt1)
declaration io w st = do
  e <- Exception.try $ \ex -> do
    safePeek st >>= \case
      Just (TokenType.Token TokenType.CLASS _ _) -> advance st >> classDeclaration io w ex st
      Just (TokenType.Token TokenType.FUNN _ _) -> advance st >> functionDeclaration io w ex st
      Just (TokenType.Token TokenType.VAR _ _) -> advance st >> varDeclaration io w ex st
      _ -> statement io w ex st
  case e of
    Left _ -> synchronize st >> pure Nothing
    Right v -> pure $ Just v

{-# INLINE classDeclaration #-}
classDeclaration
  :: forall es io w ex st
   . (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
classDeclaration io w ex st = do
  name <- consume io w ex st TokenType.IDENTIFIER "Expect class name."
  superclass <-
    safePeek st >>= \case
      Just (TokenType.Token TokenType.LESS _ _) ->
        advance st
          >> Just . (,())
            <$> consume io w ex st TokenType.IDENTIFIER "Expect superclass name."
      _ -> pure Nothing
  consume io w ex st TokenType.LEFT_BRACE "Expect '{' before class body.."
  methods <- getMethods VB.empty
  consume io w ex st TokenType.RIGHT_BRACE "Expect '}' after class body."
  pure $ Stmt.SClass name () superclass methods
  where
    getMethods
      :: VB.Builder Stmt.FunctionH1
      -> Eff es (Vector Stmt.FunctionH1)
    getMethods accum = do
      not <$> check st TokenType.RIGHT_BRACE >>= \case
        -- if we havent found RIGHT_BRACE, we try parsing class methods
        True -> do
          nextMethod <- function io w ex st "method"
          getMethods (accum <> VB.singleton nextMethod)
        False -> pure $ VB.build accum

{-# INLINE functionDeclaration #-}
functionDeclaration
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
functionDeclaration io w ex st = Stmt.SFunction <$> function io w ex st "function"

{-# INLINEABLE function #-}
function
  :: forall es io w ex st
   . (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> ByteString
  -> Eff es Stmt.FunctionH1
function io w ex st kind = do
  name <- consume io w ex st TokenType.IDENTIFIER $ "Expect " <> kind <> " name."
  consume io w ex st TokenType.LEFT_PAREN $
    "Expect '(' after " <> kind <> " name."
  parameters <-
    not <$> check st TokenType.RIGHT_PAREN >>= \case
      -- if we haven't found yet a RIGHT_PAREN (ex. 'fun name(1, arg1)')
      True -> getParams VB.empty
      -- if we have found a RIGHT_PAREN (ex. 'fun name()')
      False -> pure V.empty
  consume io w ex st TokenType.RIGHT_PAREN "Expect ')' after parameters."
  consume io w ex st TokenType.LEFT_BRACE $
    "Expect '{' before " <> kind <> " body."
  Stmt.FFunctionH name () parameters <$> block io w ex st
  where
    getParams
      :: VB.Builder TokenType.Token
      -> Eff es (Vector TokenType.Token)
    getParams accum = do
      when (VB.size accum >= 255) $ do
        peek st >>= \tok ->
          void $ getAndReportParserError io w tok "Cant have more than 255 parameters."
      nextParamName <-
        consume io w ex st TokenType.IDENTIFIER "Expect parameter name."
      match st [TokenType.COMMA] >>= \case
        True -> getParams (accum <> VB.singleton nextParamName)
        False -> pure $ VB.build (accum <> VB.singleton nextParamName)

statement
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
statement io w ex st =
  safePeek st >>= \case
    Just (TokenType.Token TokenType.FOR _ _) -> advance st >> forStatement io w ex st
    Just (TokenType.Token TokenType.IF _ _) -> advance st >> ifStatement io w ex st
    Just (TokenType.Token TokenType.PRINT _ _) -> advance st >> printStatement io w ex st
    Just (TokenType.Token TokenType.RETURN _ _) -> advance st >> returnStatement io w ex st
    Just (TokenType.Token TokenType.WHILE _ _) -> advance st >> whileStatement io w ex st
    Just (TokenType.Token TokenType.LEFT_BRACE _ _) -> advance st >> (Stmt.SBlock <$> block io w ex st)
    _ -> expressionStatement io w ex st

returnStatement
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
returnStatement io w ex st = do
  keyword <- previous st
  value <-
    not <$> check st TokenType.SEMICOLON >>= \case
      -- if we do not find a semicolon immediately, there is an expression
      True -> Just <$> expression io w ex st
      False -> pure Nothing
  consume io w ex st TokenType.SEMICOLON "Expect ';' after return value."
  pure $ Stmt.SReturn keyword value

forStatement
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
forStatement io w ex st = do
  consume io w ex st TokenType.LEFT_PAREN "Expect '(' after 'for'."
  initializer <-
    safePeek st >>= \case
      Just (TokenType.Token TokenType.SEMICOLON _ _) -> advance st >> pure Nothing
      Just (TokenType.Token TokenType.VAR _ _) -> advance st >> Just <$> varDeclaration io w ex st
      _ -> Just <$> expressionStatement io w ex st
  condition <-
    not <$> check st TokenType.SEMICOLON >>= \case
      True -> expression io w ex st
      -- if there's no condition, it's an infinite loop
      False -> pure $ Expr.ELiteral $ LBool True
  consume io w ex st TokenType.SEMICOLON "Expect ';' after loop condition."
  increment <-
    not <$> check st TokenType.RIGHT_PAREN >>= \case
      True -> Just <$> expression io w ex st
      False -> pure Nothing
  consume io w ex st TokenType.RIGHT_PAREN "Expect ')' after for clauses."
  whileStmtBody <-
    statement io w ex st <&> \forStmtBody -> case increment of
      -- if there's an increment in the 'for', add it to the end of the 'while' body
      Just inc -> Stmt.SBlock $ V.fromList [forStmtBody, Stmt.SExpression inc]
      Nothing -> forStmtBody
  -- while statement is built from the condition and the desugared for body & increment
  let whileStmt = Stmt.SWhile condition whileStmtBody
  -- finally, if there is an initializer (ex. 'for (var a = 1...)'), we prefix
  -- it to the desugared while loop
  let finishedForStmt = case initializer of
        Just ini -> Stmt.SBlock $ V.fromList [ini, whileStmt]
        Nothing -> whileStmt
  pure finishedForStmt

whileStatement
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
whileStatement io w ex st = do
  consume io w ex st TokenType.LEFT_PAREN "Expect '(' after 'while'."
  condition <- expression io w ex st
  consume io w ex st TokenType.RIGHT_PAREN "Expect ')' after condition."
  Stmt.SWhile condition <$> statement io w ex st

ifStatement
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
ifStatement io w ex st = do
  consume io w ex st TokenType.LEFT_PAREN "Expect '(' after 'if'."
  condition <- expression io w ex st
  consume io w ex st TokenType.RIGHT_PAREN "Expect ')' after if condition."
  thenBranch <- statement io w ex st
  elseBranch <-
    match st [TokenType.ELSE] >>= \case
      True -> Just <$> statement io w ex st
      False -> pure Nothing
  pure $ Stmt.SIf condition thenBranch elseBranch

block
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es (Vector Stmt.Stmt1)
block io w ex st =
  go VB.empty
    <* consume io w ex st TokenType.RIGHT_BRACE "Expect '}' after block."
  where
    go accum = do
      cond <-
        liftA2 (&&) (not <$> check st TokenType.RIGHT_BRACE) (not <$> isAtEnd st)
      if cond
        then do
          next <- declaration io w st
          case next of
            Just n -> go (accum <> VB.singleton n)
            Nothing -> go accum
        else pure (VB.build accum)

printStatement
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
printStatement io w ex st = do
  value <- expression io w ex st
  consume io w ex st TokenType.SEMICOLON "Expect ';' after value."
  pure $ Stmt.SPrint value

expressionStatement
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Stmt.Stmt1
expressionStatement io w ex st = do
  expr <- expression io w ex st
  consume io w ex st TokenType.SEMICOLON "Expect ';' after expression."
  pure $ Stmt.SExpression expr

expression
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
expression = assignment

assignment
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
assignment io w ex st = do
  expr <- orP io w ex st
  m <- match st [TokenType.EQUAL]
  if m
    then do
      equals <- previous st
      value <- assignment io w ex st
      case expr of
        Expr.EVariable name _ -> pure $ Expr.EAssign name value ()
        Expr.EGet object name -> pure $ Expr.ESet object name value
        _ -> do
          -- the error is reported, but does not need to synchronize, hence why there's no `throwError`
          getAndReportParserError io w equals "Invalid assignment target."
          pure expr
    else pure expr

orP
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
orP io w ex st = andP io w ex st >>= whileParseELogical st (andP io w ex st) [TokenType.OR]

andP
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
andP io w ex st =
  equality io w ex st
    >>= whileParseELogical st (equality io w ex st) [TokenType.AND]

equality
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
equality io w ex st =
  comparison io w ex st
    >>= whileParseEBinary
      st
      (comparison io w ex st)
      [TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL]

comparison
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
comparison io w ex st =
  term io w ex st
    >>= whileParseEBinary
      st
      (term io w ex st)
      [ TokenType.GREATER
      , TokenType.GREATER_EQUAL
      , TokenType.LESS
      , TokenType.LESS_EQUAL
      ]

term
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
term io w ex st =
  factor io w ex st
    >>= whileParseEBinary
      st
      (factor io w ex st)
      [TokenType.MINUS, TokenType.PLUS]

factor
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
factor io w ex st =
  unary io w ex st
    >>= whileParseEBinary st (unary io w ex st) [TokenType.SLASH, TokenType.STAR]

unary
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
unary io w ex st = do
  match st [TokenType.BANG, TokenType.MINUS] >>= \m ->
    if m
      then liftA2 EUnary (previous st) (unary io w ex st)
      else
        call io w ex st

call
  :: forall es io w ex st
   . (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
call io w ex st = do
  expr <- primary io w ex st
  whileTrue expr
  where
    whileTrue
      :: Expr1
      -> Eff es Expr1
    whileTrue prev = do
      safePeek st >>= \case
        -- if we find a LEFT_PAREN after a primary expr, this is a function call
        Just (TokenType.Token TokenType.LEFT_PAREN _ _) ->
          advance st >> finishCall prev >>= whileTrue
        Just (TokenType.Token TokenType.DOT _ _) -> do
          advance st
          name <-
            consume io w ex st TokenType.IDENTIFIER "Expect property name after '.'."
          whileTrue $ EGet prev name
        _ ->
          pure prev
    finishCall
      :: Expr1
      -> Eff es Expr1
    finishCall callee = do
      arguments <-
        not <$> check st TokenType.RIGHT_PAREN >>= \case
          -- if we have'nt found yet a RIGHT_PAREN
          True -> getArgs 0 VB.empty
          -- if we have found a RIGHT_PAREN
          False -> pure V.empty
      paren <- consume io w ex st TokenType.RIGHT_PAREN "Expect ')' after arguments."
      pure $ ECall callee paren arguments
      where
        getArgs
          :: Int
          -> VB.Builder Expr1
          -> Eff es (Vector Expr1)
        getArgs argCount accum = do
          when (argCount >= 255) $ do
            peek st >>= \tok ->
              void $ getAndReportParserError io w tok "Can't have more than 255 arguments."
          nextArg <- expression io w ex st
          match st [TokenType.COMMA] >>= \case
            True -> getArgs (argCount + 1) (accum <> VB.singleton nextArg)
            False -> pure $ VB.build (accum <> VB.singleton nextArg)

primary
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> Eff es Expr1
primary io w ex st = do
  t <- safePeek st
  case t of
    Just (TokenType.Token TokenType.FALSE _ _) -> advance st >> pure (ELiteral $ LBool False)
    Just (TokenType.Token TokenType.TRUE _ _) -> advance st >> pure (ELiteral $ LBool True)
    Just (TokenType.Token TokenType.NIL _ _) -> advance st >> pure (ELiteral LNil)
    Just (TokenType.Token (TokenType.NUMBER lit) _ _) -> advance st >> pure (ELiteral $ LNumber lit)
    Just (TokenType.Token (TokenType.STRING lit) _ _) -> advance st >> pure (ELiteral $ LString lit)
    Just keyword@(TokenType.Token TokenType.SUPER _ _) -> do
      advance st >> consume io w ex st TokenType.DOT "Expect '.' after 'super'."
      method <-
        consume io w ex st TokenType.IDENTIFIER "Expect superclass method name."
      pure $ ESuper keyword () method
    Just tok@(TokenType.Token TokenType.THIS _ _) -> advance st >> pure (EThis tok ())
    Just tok@(TokenType.Token TokenType.IDENTIFIER _ _) -> advance st >> pure (EVariable tok ())
    Just (TokenType.Token TokenType.LEFT_PAREN _ _) ->
      advance st >> do
        expr <- expression io w ex st
        consume io w ex st TokenType.RIGHT_PAREN "Expect ')' after expression."
        pure $ EGrouping expr
    _ -> do
      p <- peek st
      getAndReportParserError io w p "Expect expression." >>= Exception.throw ex

safePeek
  :: st :> es
  => State Parser st
  -> Eff es (Maybe TokenType.Token)
safePeek st = do
  c <- not <$> isAtEnd st
  if c then Just <$> peek st else pure Nothing

{-# INLINE whileParse #-}
whileParse
  :: st :> es
  => State Parser st
  -> (Expr1 -> TokenType.Token -> Expr1 -> Expr1)
  -> Eff es Expr1
  -> [TokenType.TokenType]
  -> Expr1
  -> Eff es Expr1
whileParse st constr = go
  where
    go inner matchlist eAccum = do
      m <- match st matchlist
      if m
        then do
          operator <- previous st
          right <- inner
          go inner matchlist $ constr eAccum operator right
        else
          pure eAccum

whileParseEBinary
  :: st :> es
  => State Parser st
  -> Eff es Expr1
  -> [TokenType.TokenType]
  -> Expr1
  -> Eff es Expr1
whileParseEBinary st = whileParse st EBinary

whileParseELogical
  :: st :> es
  => State Parser st
  -> Eff es Expr1
  -> [TokenType.TokenType]
  -> Expr1
  -> Eff es Expr1
whileParseELogical st = whileParse st ELogical

synchronize
  :: forall es st
   . st :> es
  => State Parser st
  -> Eff es ()
synchronize st = advance st >> go
  where
    go :: Eff es ()
    go = do
      e <- not <$> isAtEnd st
      if e
        then do
          t <- previous st
          p <- peek st
          if t.ttype == TokenType.SEMICOLON
            then pure ()
            else case p.ttype of
              TokenType.CLASS -> pure ()
              TokenType.FUNN -> pure ()
              TokenType.VAR -> pure ()
              TokenType.FOR -> pure ()
              TokenType.IF -> pure ()
              TokenType.WHILE -> pure ()
              TokenType.PRINT -> pure ()
              TokenType.RETURN -> pure ()
              _ -> advance st >> go
        else do
          pure ()

consume
  :: (io :> es, w :> es, ex :> es, st :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> Exception ParseException ex
  -> State Parser st
  -> TokenType.TokenType
  -> ByteString
  -> Eff es TokenType.Token
consume io w ex st ttype message =
  check st ttype >>= \c ->
    if c
      then advance st
      else
        peek st
          >>= (\t -> getAndReportParserError io w t message)
          >>= Exception.throw ex

getAndReportParserError
  :: (io :> es, w :> es)
  => IOE io
  -> Writer Error.ErrorPresent w
  -> TokenType.Token
  -> ByteString
  -> Eff es ParseException
getAndReportParserError io w token message = do
  Error.parseError io w token message
  pure ParseException

match
  :: st :> es
  => State Parser st
  -> [TokenType.TokenType]
  -> Eff es Bool
match st = \case
  [] -> pure False
  (ttype : ttypes) ->
    check st ttype >>= \c ->
      if c
        then advance st >> pure True
        else match st ttypes

check
  :: st :> es
  => State Parser st
  -> TokenType.TokenType
  -> Eff es Bool
check st ttype =
  isAtEnd st >>= \c ->
    if c
      then pure False
      else do
        p <- peek st
        pure (p.ttype == ttype)

advance
  :: st :> es
  => State Parser st
  -> Eff es TokenType.Token
advance st = do
  c <- not <$> isAtEnd st
  when c $ State.modify st $ \pr -> pr {current = pr.current + 1}
  previous st

isAtEnd
  :: st :> es
  => State Parser st
  -> Eff es Bool
isAtEnd st = (\t -> t.ttype == TokenType.EOF) <$> peek st

-- TODO: unsafeIndex ?
peek
  :: st :> es
  => State Parser st
  -> Eff es TokenType.Token
peek st = State.get st <&> \pr -> pr.tokens V.! pr.current

previous
  :: st :> es
  => State Parser st
  -> Eff es TokenType.Token
previous st = State.get st <&> \pr -> pr.tokens V.! (pr.current - 1)
