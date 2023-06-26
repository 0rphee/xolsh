module Main (main) where

import CmdlineOptions

main :: IO ()
main = do
  (Options sourceCodeFilepath) <- execParser options
  putStrLn "Hello world"
