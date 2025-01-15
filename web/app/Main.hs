module Main (main, plus3) where

import GHC.Wasm.Prim

main :: IO ()
main =
  putStrLn $ "Hello, Haskell! "

foreign export javascript "plus3"
  plus3 :: Word -> Word

foreign export javascript "main2"
  main :: IO ()

plus3 :: Word -> Word
plus3 = (+ 3)
