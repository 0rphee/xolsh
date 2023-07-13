module Main (main) where

import CmdlineOptions
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Data.ByteString.Char8 as B
import Data.Function ((&))
import System.IO (stderr)

newtype Error = Err Bool

main :: IO ()
main = do
  (Options sourceCodeFilepath) <- execParser options
  maybe runPrompt runFile sourceCodeFilepath

runFile :: ByteString -> StateT Error IO ()
runFile path = do
  liftIO $ B.putStrLn ("Run file: " <> path)
  fileContents <- liftIO $ B.readFile (B.unpack path)
  liftIO $ run fileContents

runPrompt :: StateT Error IO ()
runPrompt = forever $ do
  liftIO $ B.putStr "> "
  line <- liftIO B.getLine
  liftIO $
    if B.null line
      then B.putStr "\n"
      else run line

run :: ByteString -> IO ()
run source = do
  let tokens = scan source
  forM_ tokens B.putStrLn

scan :: ByteString -> [ByteString]
scan = undefined

reportError :: Int -> ByteString -> StateT Error IO ()
reportError lineNum message = do
  liftIO $
    B.hPutStrLn stderr $
      "[line " <> (lineNum & show & B.pack) <> "] Error: " <> message
  put (Err True)
