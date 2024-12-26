{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import CmdlineOptions qualified
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Foldable (traverse_)
import Data.IORef qualified as IORef
import Scanner
import System.Exit qualified

newtype Global = Global {unGlobal :: IORef.IORef Bool}

main :: IO ()
main = do
  (CmdlineOptions.Options sourceCodeFilepath) <-
    CmdlineOptions.execParser CmdlineOptions.options

  errRef <- liftIO $ Global <$> IORef.newIORef False
  (`runReaderT` errRef) $ case sourceCodeFilepath of
    Nothing -> runPrompt
    Just sourceCodeFile -> runFile sourceCodeFile

runFile :: MonadIO m => ByteString -> ReaderT Global m ()
runFile path = do
  liftIO $ B.putStrLn ("Run file: " <> path)
  fileContents <- liftIO $ B.readFile (B.unpack path)
  run fileContents
  hadError <- ask >>= (liftIO . IORef.readIORef . (.unGlobal))
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
      ask >>= \ref -> liftIO $ IORef.writeIORef ref.unGlobal False

run :: MonadIO m => ByteString -> ReaderT Global m ()
run sourceBS = do
  let (tokens, errs) = scanTokens sourceBS
  liftIO $ printRes tokens
  if null errs
    then
      pure ()
    else do
      liftIO $ B.putStrLn ""
      liftIO $ printRes errs
  where
    printRes :: (Show a, Traversable t) => t a -> IO ()
    printRes = traverse_ print
