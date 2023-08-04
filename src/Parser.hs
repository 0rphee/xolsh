{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Parser
  ( Parser (..)
  , Result (..)
  , runParser
  , parsePrimary
  , advance
  , takeNTokens
  , satisfyP
  , skip
  , headP
  , skipSatisfyP
  , errP
  , parseComparison
  , parseEquality
  , parseExpression
  , branchP
  , parseFactor
  , parseTerm
  , parseUnary
  , failP
  )
where

import Control.Applicative
import Control.Monad (unless)
import Data.Vector as V
import Expr
import Token

newtype Parser e r = Parser {runParser_ :: Int -> Vector Token -> Result e r}

data Result e a
  = OK !a !Int !(Vector Token)
  | Fail
  | Err !e
  deriving (Show, Functor)

instance Functor (Parser e) where
  fmap f (Parser g) = Parser \i v -> f <$> g i v

instance Applicative (Parser e) where
  pure x = Parser $ OK x
  (Parser p) <*> (Parser g) = Parser \i v -> do
    case p i v of
      OK a i' restV -> a <$> g i' restV
      Fail -> Fail
      Err e -> Err e
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad (Parser e) where
  return = pure
  (Parser p) >>= g = Parser \i v -> do
    case p i v of
      OK a i' restV -> runParser_ (g a) i' restV
      Fail -> Fail
      Err e -> Err e
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance Alternative (Parser e) where
  empty = Parser $ \_ _ -> Fail
  (Parser p1) <|> (Parser p2) = Parser $ \i input ->
    case p1 i input of
      OK result i' rest -> OK result i' rest
      Fail -> p2 i input
      x -> x
  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}

runParser :: Parser e r -> Vector Token -> Result e r
runParser p = runParser_ p 0

-- | The failing parser.
failP :: Parser e r
failP = Control.Applicative.empty
{-# INLINE failP #-}

-- a b c d
-- 0 1 2 3

-- n = 2
-- length = 4
-- pos = 3
-- newI = pos + 1 =4

-- if 4 - 1 > 2
-- then OK () 3
-- else Fail

-- | Skip n# items. Fails if fewer than n# items are available.
skip :: Int -> Parser e ()
skip n = Parser \i v ->
  let offset = i + n
   in if (V.length v - i) > n
        then OK () offset v
        else Fail
{-# INLINE skip #-}

advance :: Parser e ()
advance = skip 1
{-# INLINE advance #-}

-- | Read n# items. Fails if fewer than n# items are available.
takeNTokens :: Int -> Parser e (Vector Token)
takeNTokens n = Parser \i v ->
  let offset = i + n
   in if (V.length v - i) > n
        then OK (V.slice i offset v) offset v
        else Fail
{-# INLINE takeNTokens #-}

-- | Read next item. Fails if the vector is empty.
headP :: Parser e Token
headP = Parser $ \i v ->
  if V.length v > i
    then OK (v V.! i) (i + 1) v
    else Fail
{-# INLINE headP #-}

-- | Reads the next item that satisfies the condition. Fails otherwise.
satisfyP :: (Token -> Bool) -> Parser e Token
satisfyP f = do
  ch <- headP
  if f ch
    then pure ch
    else failP
{-# INLINE satisfyP #-}

-- | Skips the next item that satisfies the condition. Fails otherwise.
skipSatisfyP :: (Token -> Bool) -> Parser e ()
skipSatisfyP f = do
  ch <- headP
  unless (f ch) failP
{-# INLINE skipSatisfyP #-}

-- | Runs the first parser. If it succeds, runs the second, if not, runs the third.
branchP :: Parser e a -> Parser e b -> Parser e b -> Parser e b
branchP f g h = Parser \i v ->
  case runParser_ f i v of
    OK _ i' rest -> runParser_ g i' rest
    Fail -> runParser_ h i v
    Err e -> Err e
{-# INLINE branchP #-}

-- | Throw the error 'e'.
errP :: e -> Parser e a
errP e = Parser (\_ _ -> Err e)
{-# INLINE errP #-}

parseExpression :: Parser e Expr
parseExpression = Expr <$> parseEquality

parseEquality :: Parser e EqualityExpr
parseEquality = do
  leftCompExpr <- parseComparison
  rightExprs <-
    V.fromList <$> many do
      tok <- headP
      operator <- case tok of
        EQUAL_EQUAL -> pure EEQ
        BANG_EQUAL -> pure ENOTEQ
        _ -> failP
      right <- parseComparison
      pure (operator, right)
  pure $ EqualityExpr leftCompExpr rightExprs

parseComparison :: Parser e ComparisonExpr
parseComparison = do
  leftTermExpr <- parseTerm
  rightExprs <-
    V.fromList <$> many do
      tok <- headP
      operator <- case tok of
        GREATER -> pure CGT
        GREATER_EQUAL -> pure CGTEQ
        LESS -> pure CLT
        LESS_EQUAL -> pure CLTEQ
        _ -> failP
      right <- parseTerm
      pure (operator, right)
  pure $ ComparisonExpr leftTermExpr rightExprs

parseTerm :: Parser e TermExpr
parseTerm = do
  leftFactorExpr <- parseFactor
  rightExprs <-
    V.fromList <$> many do
      tok <- headP
      operator <- case tok of
        MINUS -> pure TMinus
        PLUS -> pure TPlus
        _ -> failP
      right <- parseFactor
      pure (operator, right)
  pure $ TermExpr leftFactorExpr rightExprs

parseFactor :: Parser e FactorExpr
parseFactor = do
  leftFactorExpr <- parseUnary
  rightExprs <-
    V.fromList <$> many do
      tok <- headP
      operator <- case tok of
        SLASH -> pure FDivision
        STAR -> pure FMultiplication
        _ -> failP
      right <- parseUnary
      pure (operator, right)
  pure $ FactorExpr leftFactorExpr rightExprs

parseUnary :: Parser e UnaryExpr
parseUnary =
  let first = do
        tok <- headP
        a <- case tok of
          BANG -> pure UNegate
          MINUS -> pure UMinus
          _ -> failP
        UnaryExpr a <$> parseUnary
   in first <|> (UPrimaryExpr <$> parsePrimary)

parsePrimary :: Parser e PrimaryExpr
parsePrimary =
  headP >>= \case
    FALSE -> pure $ PBoolConstExpr False
    TRUE -> pure $ PBoolConstExpr True
    NIL -> pure PNilExpr
    STRING bs -> pure $ PStrExpr bs
    NUMBER num -> pure $ PNumberExpr num
    LEFT_PAREN -> do
      expr <- parseExpression
      branchP
        (skipSatisfyP (== RIGHT_PAREN))
        (pure $ PGroupedExpr expr)
        (errP undefined) -- TODO add error handling
    _ -> failP
