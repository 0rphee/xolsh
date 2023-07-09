module Main (main) where

import CmdlineOptions
import Data.ByteString.Char8 as B
import System.Exit

main :: IO ()
main = do
  (Options sourceCodeFilepath) <- execParser options
  case sourceCodeFilepath of
    Just path -> do
      B.putStrLn $ "Run file: " <> path
    Nothing -> do
      B.putStrLn "Start repl"
      exitWith (ExitFailure 64)
