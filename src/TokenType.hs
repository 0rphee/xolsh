{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}

module TokenType
  ( TokenType (..)
  , Token (..)
  -- Literal (..)
  )
where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
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
  | STRING !ByteString
  | NUMBER !Double
  | -- Keywords.
    AND
  | CLASS
  | ELSE
  | FALSE
  | -- data constructors named 'FUN'  are problematic when using TH, see: https://gitlab.haskell.org/ghc/ghc/-/issues/20902
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

-- data Literal
--   = LitStr !ByteString
--   | LitNum !Double
--   | LitBool !Bool
--   | LitNil
--   deriving (Show)

data Token = Token
  { ttype :: !TokenType
  , lexeme :: !ByteString
  , --   , literal :: !Literal -- Object in jlox reference implementation, removed here and moved to @TokenType@
    tline :: !Int
  }
  deriving (Show, Lift)

-- instance Show Token where
--   show tok = show tok.ttype <> " " <> BS.unpack tok.lexeme <> " " -- <> show tok.literal
