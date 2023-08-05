module Main (main) where

import Control.Applicative
import Data.Vector as V
import Expr
import Parser
import Test.Tasty.Bench
import Token

toks :: Vector Token
toks =
  V.fromList $
    badPosTok
      <$> [FALSE, TRUE, NIL, STRING "aa", NUMBER 56.0]

bParsePrimaryExpr :: Vector Token -> [PrimaryExpr]
bParsePrimaryExpr t = case runParser (many parsePrimary) t of
  OK !xs _ -> xs
  _ -> undefined

main :: IO ()
main =
  defaultMain
    [ bench "parsePrimary :: Parser e PrimaryExpr" $ nf bParsePrimaryExpr toks
    ]