{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

module TokenType where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS

data TokenType
  = -- Single-character tokens.
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | -- One or two character tokens.
    BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | -- Literals.
    IDENTIFIER
  | STRING
  | NUMBER
  | -- Keywords.
    AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
  deriving (Show)

data Literal
  = NoLit
  | LitStr ByteString
  | LitNum Double
  deriving (Show)

data Token = Token
  { ttype :: TokenType
  , lexeme :: ByteString
  , literal :: Literal {-Object in java-}
  , tline :: Int
  }

instance Show Token where
  show tok = show tok.ttype <> " " <> BS.unpack tok.lexeme <> " " <> show tok.literal
