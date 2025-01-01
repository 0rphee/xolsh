{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AstPrinter (printAst) where

import Data.ByteString.Builder qualified as BS
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (foldl')
import Expr (Expr (..))
import TokenType (Literal (..), Token (..))

printAst :: Expr -> ByteString
printAst = BS.toStrict . BS.toLazyByteString . go
  where
    go = \case
      EBinary lexpr operator rexpr -> paren (BS.byteString operator.lexeme) [go lexpr, go rexpr]
      EGrouping expr -> paren "group" [go expr]
      ELiteral litValue ->
        case litValue of
          NoLit -> "nil"
          LitStr str -> BS.byteString str
          LitNum num -> BS.stringUtf8 $ show num
      EUnary operator expr -> paren (BS.byteString operator.lexeme) [go expr]
      where
        paren :: BS.Builder -> [BS.Builder] -> BS.Builder
        paren name s =
          "("
            <> name
            <> foldl' (\accum next -> accum <> " " <> next) mempty s
            <> ")"
