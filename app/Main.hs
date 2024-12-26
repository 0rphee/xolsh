module Main (main) where

import CmdlineOptions qualified
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Foldable (traverse_)
import Data.IORef qualified as IORef
import System.Exit qualified

newtype Global = Global {unGlobal :: IORef.IORef Bool}

main :: IO ()
main = do
  (CmdlineOptions.Options sourceCodeFilepath) <-
    CmdlineOptions.execParser CmdlineOptions.options

  errRef <- liftIO $ Global <$> IORef.newIORef False
  case sourceCodeFilepath of
    Nothing -> runPrompt
    Just sourceCodeFile -> runFile sourceCodeFile

runFile :: MonadIO m => ByteString -> ReaderT Global m ()
runFile path = do
  liftIO $ B.putStrLn ("Run file: " <> path)
  fileContents <- liftIO $ B.readFile (B.unpack path)
  run fileContents
  hadError <- liftIO $ ask >>= IORef.readIORef
  liftIO $
    if hadError
      then System.Exit.exitWith $ System.Exit.ExitFailure 65
      else System.Exit.exitSuccess

runPrompt :: MonadIO m => ReaderT Global m ()
runPrompt = forever $ do
  liftIO $ B.putStr "> "
  line <- liftIO B.getLine
  if B.null line
    then liftIO $ B.putStr "\n"
    else do
      run line
      liftIO $ ask >>= \ref -> IORef.writeIORef ref False

run :: MonadIO m => ByteString -> ReaderT Global m ()
run sourceBS = do
  let tokens = scanTokens sourceBS
  liftIO $ printRes tokens
  where
    printRes = traverse_ print

type Tokens = String

scanTokens :: MonadIO m => ByteString -> ReaderT Global m [Tokens]
scanTokens errRef = undefined
