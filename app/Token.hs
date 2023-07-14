module Token where

import Data.ByteString.Char8 as B

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
    IDENTIFIER
  | STRING
  | NUMBER
  | --    Keywords.
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

data Token = Token
  { tokType :: !TokenType
  , tokLexeme :: !ByteString
  , -- , tokLiteral :: a
    tokLine :: !Int
  }
  deriving (Show)
