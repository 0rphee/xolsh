module Main (main) where

import CmdlineOptions

main :: IO ()
main = do
  (Options sourceCodeFilepath) <- execParser options
  case sourceCodeFilepath of
    Just path -> putStrLn $ "run file: " ++ path
    Nothing -> putStrLn "Start repl"
