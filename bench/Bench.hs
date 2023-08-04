module Main where

import Test.Tasty.Bench

myFibo :: Int -> Integer
myFibo n = if n < 3 then toInteger n else myFibo (n - 1) + myFibo (n - 2)

main :: IO ()
main =
  defaultMain
    [ bench "myFibo 20" $ nf myFibo 20
    ]