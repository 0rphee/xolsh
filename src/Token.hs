{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Token (Token (..), TokPos) where

import Data.ByteString.Char8 as B

type TokPos =
  ( -- The Token's line
    Int
  , -- The Token's column
    Int
  )

data Token
  = --   Single-character tokens.
    LEFT_PAREN TokPos
  | RIGHT_PAREN TokPos
  | LEFT_BRACE TokPos
  | RIGHT_BRACE TokPos
  | COMMA TokPos
  | DOT TokPos
  | MINUS TokPos
  | PLUS TokPos
  | SEMICOLON TokPos
  | SLASH TokPos
  | STAR TokPos
  | --   One or two character tokens.
    BANG TokPos
  | BANG_EQUAL TokPos
  | EQUAL TokPos
  | EQUAL_EQUAL TokPos
  | GREATER TokPos
  | GREATER_EQUAL TokPos
  | LESS TokPos
  | LESS_EQUAL TokPos
  | --   Literals.
    IDENTIFIER TokPos
  | STRING !ByteString TokPos
  | NUMBER !Double TokPos
  | --    Keywords.
    AND TokPos
  | CLASS TokPos
  | ELSE TokPos
  | FALSE TokPos
  | FUNN TokPos
  | FOR TokPos
  | IF TokPos
  | NIL TokPos
  | OR TokPos
  | PRINT TokPos
  | RETURN TokPos
  | SUPER TokPos
  | THIS TokPos
  | TRUE TokPos
  | VAR TokPos
  | WHILE TokPos
  | EOF TokPos
  deriving (Show, Eq)

-- data Token = Token
--   { tokType :: !TokenType
--   , tokLexeme :: !(Maybe ByteString)
--   -- , -- , tokLiteral :: a
--   -- tokLine :: !Int
--   }
--   deriving (Show)
