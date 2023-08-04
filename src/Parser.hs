{-# LANGUAGE BlockArguments #-}

module Parser () where

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

satisfyP :: (Token -> Bool) -> Parser e Token
satisfyP f = do
  ch <- headP
  if f ch
    then pure ch
    else failP
{-# INLINE satisfyP #-}

skipSatisfyP :: (Token -> Bool) -> Parser e ()
skipSatisfyP f = do
  ch <- headP
  unless (f ch) failP
{-# INLINE skipSatisfyP #-}
