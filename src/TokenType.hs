{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}

module TokenType
  ( TokenType (..)
  , Token (..)
  )
where

import Data.ByteString.Short (ShortByteString)
import Language.Haskell.TH.Syntax (Lift)

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
  | STRING !ShortByteString
  | NUMBER !Double
  | -- Keywords.
    AND
  | CLASS
  | ELSE
  | FALSE
  | -- data constructors named 'FUN' are problematic when using TH, see: https://gitlab.haskell.org/ghc/ghc/-/issues/20902
    FUNN
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
  deriving (Eq, Show, Lift)

data Token = Token
  { ttype :: !TokenType
  , lexeme :: !ShortByteString
  , tline :: !Int
  }
  deriving (Show, Lift)
