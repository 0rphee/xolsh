{-# LANGUAGE DataKinds #-}

module Main (main) where

import CmdlineOptions qualified as CmdLine
import Control.Monad (forM_, forever, when)
import Control.Monad.State.Strict
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Vector (Vector)
import Diagnostics (printErrors)
import Scanner qualified as S
import System.Exit (ExitCode (..), exitWith)
import Token

newtype AppState
  = -- | Whether there are errors or not.
    StateErr Bool

-- Define a monad stack using MTL
newtype AppM a
  = AppM (StateT AppState IO a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadState AppState)

runAppM :: AppM a -> IO a
runAppM (AppM x) = evalStateT x initialState
  where
    initialState = StateErr False

main :: IO ()
main = do
  (CmdLine.Options sourceCodeFilepath) <- CmdLine.execParser CmdLine.options
  case sourceCodeFilepath of
    Nothing -> runAppM runPrompt
    Just sourceCodeFile -> runAppM $ runFile sourceCodeFile

runFile :: (MonadIO m, MonadState AppState m) => FilePath -> m ()
runFile path = do
  liftIO $ putStrLn ("Run file: " <> path)
  fileContents <- liftIO $ B.readFile path
  run path fileContents
  (StateErr err) <- get
  when err (liftIO $ exitWith (ExitFailure 65))

runPrompt :: MonadIO m => m ()
runPrompt = forever $ do
  liftIO $ B.putStr "> "
  line <- liftIO B.getLine
  liftIO $
    if B.null line
      then B.putStr "\n"
      else run "shell" line

run :: MonadIO m => FilePath -> ByteString -> m ()
run path sourceBS = do
  let tokens = S.runScanFile sourceBS
  liftIO $ printRes tokens
  where
    printRes
      :: (Vector (S.ScannerError S.AsPos), Maybe (Vector Token, ByteString))
      -> IO ()
    printRes (errVec, toks) = case toks of
      Just (v, _restOfBS) ->
        let p = B.putStrLn . B.pack . show
         in do
              printErrors path sourceBS errVec
              forM_ v p
      -- B.putStrLn ("Rest of BS: " <> restOfBS) NOTE: it's been a long time since i've seen a non-empty bytestring out of the scan function
      Nothing -> printErrors path sourceBS errVec
