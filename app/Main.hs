module Main (main) where

import CmdlineOptions
import Data.ByteString.Char8 as B

main :: IO ()
main = do
  (Options sourceCodeFilepath) <- execParser options
  maybe runPrompt runFile sourceCodeFilepath

runFile :: ByteString -> IO ()
runFile path = do
  B.putStrLn $ "Run file: " <> path
  fileContents <- B.readFile $ B.unpack path
  run fileContents

run :: ByteString -> IO ()
run b = do
  B.putStrLn b
  undefined

runPrompt :: IO ()
runPrompt = B.putStrLn "Run prompt"
