{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AstPrinter (printAst) where

import Data.ByteString.Builder qualified as BS
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (foldl')
import Expr (Expr (..), LiteralValue (..))
import TokenType (Token (..))

printAst :: Expr a -> ByteString
printAst = BS.toStrict . BS.toLazyByteString . go
  where
    go = \case
      EBinary lexpr operator rexpr -> paren (BS.shortByteString operator.lexeme) [go lexpr, go rexpr]
      EGrouping expr -> paren "group" [go expr]
      ELiteral litValue ->
        case litValue of
          LNil -> "nil"
          LString str -> BS.shortByteString str
          LNumber num -> BS.stringUtf8 $ show num
          LBool b -> (if b then "true" else "false")
          _ -> "litval unimplemented!"
      EUnary operator expr -> paren (BS.shortByteString operator.lexeme) [go expr]
      _ -> "expr unimplemented!"
      where
        paren :: BS.Builder -> [BS.Builder] -> BS.Builder
        paren name s =
          "("
            <> name
            <> foldl' (\accum next -> accum <> " " <> next) mempty s
            <> ")"
