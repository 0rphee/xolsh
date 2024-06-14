module Token (TokenType (..), Token (..)) where

import Data.ByteString.Char8 as B
import FlatParse.Stateful (Pos)

data TokenType
  = --   Single-character tokens.
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
  | --   One or two character tokens.
    BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | --   Literals.
    IDENTIFIER !ByteString
  | STRING !ByteString
  | NUMBER !Double
  | --    Keywords.
    AND
  | CLASS
  | ELSE
  | FALSE
  | FUNN
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
  deriving (Show, Eq)

data Token = Token
  { tokType :: !TokenType
  , tokPos :: Pos
  }
  deriving (Show)
