module Main (main) where

import CmdlineOptions
import Control.Monad (forM_, forever)
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

runPrompt :: IO ()
runPrompt = forever $ do
  B.putStr "> "
  line <- B.getLine
  if B.null line
    then B.putStr "\n"
    else run line

run :: ByteString -> IO ()
run source = do
  let scan = undefined :: ByteString -> [ByteString]
  let tokens = scan source
  forM_ tokens B.putStrLn

scan :: ByteString -> [ByteString]
scan = undefined
