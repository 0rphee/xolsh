module Token (Token (..), TokPos, badPosTok) where

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
  deriving (Show)

instance Eq Token where
  (==) = myEq

myEq :: Token -> Token -> Bool
myEq l r = case (l, r) of
  (LEFT_PAREN _, LEFT_PAREN _) -> True
  (RIGHT_PAREN _, RIGHT_PAREN _) -> True
  (LEFT_BRACE _, LEFT_BRACE _) -> True
  (RIGHT_BRACE _, RIGHT_BRACE _) -> True
  (COMMA _, COMMA _) -> True
  (DOT _, DOT _) -> True
  (MINUS _, MINUS _) -> True
  (PLUS _, PLUS _) -> True
  (SEMICOLON _, SEMICOLON _) -> True
  (SLASH _, SLASH _) -> True
  (STAR _, STAR _) -> True
  (BANG _, BANG _) -> True
  (BANG_EQUAL _, BANG_EQUAL _) -> True
  (EQUAL _, EQUAL _) -> True
  (EQUAL_EQUAL _, EQUAL_EQUAL _) -> True
  (GREATER_EQUAL _, GREATER_EQUAL _) -> True
  (GREATER _, GREATER _) -> True
  (LESS _, LESS _) -> True
  (LESS_EQUAL _, LESS_EQUAL _) -> True
  (IDENTIFIER _, IDENTIFIER _) -> True
  (STRING _ s1, STRING _ s2) -> s1 == s2
  (NUMBER _ n1, NUMBER _ n2) -> n1 == n2
  (AND _, AND _) -> True
  (CLASS _, CLASS _) -> True
  (ELSE _, ELSE _) -> True
  (FALSE _, FALSE _) -> True
  (FUNN _, FUNN _) -> True
  (FOR _, FOR _) -> True
  (IF _, IF _) -> True
  (NIL _, NIL _) -> True
  (OR _, OR _) -> True
  (PRINT _, PRINT _) -> True
  (RETURN _, RETURN _) -> True
  (SUPER _, SUPER _) -> True
  (THIS _, THIS _) -> True
  (TRUE _, TRUE _) -> True
  (VAR _, VAR _) -> True
  (WHILE _, WHILE _) -> True
  (EOF _, EOF _) -> True
  _ -> False

badPosTok :: (TokPos -> Token) -> Token
badPosTok con = con (0, 0)
{-# INLINE badPosTok #-}

-- data Token = Token
--   { tokType :: !TokenType
--   , tokLexeme :: !(Maybe ByteString)
--   -- , -- , tokLiteral :: a
--   -- tokLine :: !Int
--   }
--   deriving (Show)
