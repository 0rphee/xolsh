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

newtype Parser e r = Parser {runParser :: Vector Token -> Result e r}

data Result e a
  = OK !a !(Vector Token)
  | Fail
  | Err !e
  deriving (Functor)

instance Functor (Parser e) where
  fmap f (Parser g) = Parser (fmap f . g)

instance Applicative (Parser e) where
  pure x = Parser $ OK x
  (Parser p) <*> (Parser g) = Parser \v -> do
    case p v of
      OK a restV -> a <$> g restV
      Fail -> Fail
      Err e -> Err e
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad (Parser e) where
  return = pure
  (Parser p) >>= g = Parser \v -> do
    case p v of
      OK a restV -> runParser (g a) restV
      Fail -> Fail
      Err e -> Err e
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance Alternative (Parser e) where
  empty = Parser $ const Fail
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      OK result rest -> OK result rest
      Fail -> p2 input
      x -> x
  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}

-- | The failing parser.
failP :: Parser e r
failP = Control.Applicative.empty
{-# INLINE failP #-}

-- | Skip n# items. Fails if fewer than n# items are available.
skip :: Int -> Parser e ()
skip n = Parser \v ->
  let rest = V.drop n v
   in if V.length v > n
        then OK () rest
        else Fail
{-# INLINE skip #-}

advance :: Parser e ()
advance = skip 1
{-# INLINE advance #-}

-- | Read n# items. Fails if fewer than n# items are available.
takeNTokens :: Int -> Parser e (Vector Token)
takeNTokens n = Parser \v ->
  let (taken, rest) = V.splitAt n v
   in if V.length v > n
        then OK taken rest
        else Fail
{-# INLINE takeNTokens #-}

-- | Read next item. Fails if the vector is empty.
headP :: Parser e Token
headP = Parser $ \v -> case V.uncons v of
  Just (a, rest) -> OK a rest
  Nothing -> Fail
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
branchP f g h = Parser \v ->
  case runParser f v of
    OK _ rest -> runParser g rest
    Fail -> runParser h v
    Err e -> Err e
{-# INLINE branchP #-}

-- | Throw the error 'e'.
errP :: e -> Parser e a
errP e = Parser (const $ Err e)
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
