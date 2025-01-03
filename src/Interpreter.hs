{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Interpreter (evaluate, interpret) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict (StateT (..), evalStateT)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Error qualified
import Expr qualified
import TokenType qualified

data InterpreterState = InterpreterState

{- |
Removing newtypes, @InterpreterM a@ is equivalent to:
  + @StateT InterpreterState Identitity (Either RuntimeError a)@
  + @InterpreterState -> Identitity (Either RuntimeError a, InterpreterState)@
-}
type InterpeterM a = ExceptT Error.RuntimeError (StateT InterpreterState IO) a

evaluate :: Expr.Expr -> InterpeterM Expr.LiteralValue
evaluate = \case
  Expr.ELiteral val -> pure val
  Expr.EGrouping expr -> evaluate expr
  Expr.EUnary op expr -> do
    right <- evaluate expr
    case (op.ttype, right) of
      (TokenType.BANG, val) -> pure $ Expr.LBool $ not $ isTruthy val
      (TokenType.MINUS, Expr.LNumber !v) -> pure $ Expr.LNumber (-v)
      (TokenType.MINUS, _) -> throwError $ Error.RuntimeError op "Operand must be a number." -- TODO
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
                Error.RuntimeError op "Operands must be two numbers or two strings." -- TODO
        TokenType.SLASH -> Expr.LNumber <$> commonIfNumber (/)
        TokenType.STAR -> Expr.LNumber <$> commonIfNumber (*)
        _ -> pure Expr.LNil -- marked as unreachable (section 7.2.5)
  where
    isTruthy :: Expr.LiteralValue -> Bool
    isTruthy = \case
      Expr.LNil -> False
      Expr.LBool v -> v
      _ -> True
    isEqual :: Expr.LiteralValue -> Expr.LiteralValue -> Bool
    isEqual l r = case (l, r) of
      -- Lox considers NaN equal to NaN, contrary to what (==) does (7.2.5)
      (Expr.LNumber vl, Expr.LNumber vr) | isNaN vl && isNaN vr -> True
      _ -> l == r

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

interpret :: Expr.Expr -> IO Error.ErrorPresent
interpret expression = do
  value <- evalStateT (runExceptT $ evaluate expression) initialInterpreterState
  case value of
    Left e -> Error.reportRuntimeError e >> pure Error.Error
    Right v -> BS.putStrLn (stringify v) >> pure Error.NoError
  where
    initialInterpreterState = InterpreterState
