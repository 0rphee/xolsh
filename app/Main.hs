{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import AstPrinter qualified
import CmdlineOptions qualified
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Foldable (traverse_)
import Data.IORef qualified as IORef
import Error qualified
import Parser (parse)
import Scanner
import System.Exit qualified

newtype Global = Global {unGlobal :: IORef.IORef Error.ErrorPresent} -- had errors: True, else False

main :: IO ()
main = do
  (CmdlineOptions.Options sourceCodeFilepath) <-
    CmdlineOptions.execParser CmdlineOptions.options

  errRef <- liftIO $ Global <$> IORef.newIORef Error.NoError
  (`runReaderT` errRef) $ case sourceCodeFilepath of
    Nothing -> runPrompt
    Just sourceCodeFile -> runFile sourceCodeFile

runFile :: MonadIO m => ByteString -> ReaderT Global m ()
runFile path = do
  liftIO $ B.putStrLn ("Run file: " <> path)
  fileContents <- liftIO $ B.readFile (B.unpack path)
  run fileContents
  hadError <- ask >>= (liftIO . IORef.readIORef . (.unGlobal))
  liftIO $ case hadError of
    Error.Error ->
      System.Exit.exitWith $ System.Exit.ExitFailure 65
    Error.NoError ->
      System.Exit.exitSuccess

runPrompt :: MonadIO m => ReaderT Global m ()
runPrompt = forever $ do
  liftIO $ B.putStr "> "
  line <- liftIO B.getLine
  if B.null line
    then liftIO $ B.putStr "\n"
    else do
      run line
      ask >>= \ref -> liftIO $ IORef.writeIORef ref.unGlobal Error.NoError

run :: MonadIO m => ByteString -> ReaderT Global m ()
run sourceBS = do
  (tokens, err1) <- liftIO $ scanTokens sourceBS
  liftIO $ traverse_ print tokens
  (m, err2) <- liftIO $ parse tokens
  case m of
    Nothing -> do
      -- lift . liftIO $ B.putStrLn "no parse success"
      pure ()
    Just r -> do
      -- lift . liftIO $ B.putStrLn "parse success"
      lift . liftIO $ B.putStrLn (AstPrinter.printAst r)
  case err1 <> err2 of
    Error.Error -> ask >>= \ref -> liftIO $ IORef.writeIORef ref.unGlobal Error.Error
    Error.NoError -> pure ()
