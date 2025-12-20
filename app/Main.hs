{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import CmdlineOptions qualified
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.IORef qualified as IORef
import Error qualified
import Interpreter (interpret)
import Optimizer (runOptimizer)
import Parser (runParse)
import Resolver (runResolver)
import Scanner
import System.Exit qualified
import System.IO (hFlush, stdout)

newtype Global = Global {unGlobal :: IORef.IORef GlobalState} -- had errors: True, else False

data GlobalState
  = GlobalState
  { hadError :: Error.ErrorPresent
  , hadRuntimeError :: Error.ErrorPresent
  }

main :: IO ()
main = do
  (CmdlineOptions.Options sourceCodeFilepath) <-
    CmdlineOptions.execParser CmdlineOptions.options

  errRef <-
    liftIO $
      Global
        <$> IORef.newIORef
          (GlobalState {hadError = Error.NoError, hadRuntimeError = Error.NoError})
  (`runReaderT` errRef) $ case sourceCodeFilepath of
    Nothing -> runPrompt
    Just sourceCodeFile -> runFile sourceCodeFile

runFile :: MonadIO m => ByteString -> ReaderT Global m ()
runFile path = do
  -- liftIO $ B.putStrLn ("Run file: " <> path)
  fileContents <- liftIO $ B.readFile (B.unpack path)
  run fileContents
  globalState <- ask >>= (liftIO . IORef.readIORef . (.unGlobal))
  liftIO $ case globalState of
    GlobalState Error.Error _ ->
      System.Exit.exitWith $ System.Exit.ExitFailure 65
    GlobalState Error.NoError Error.Error ->
      System.Exit.exitWith $ System.Exit.ExitFailure 70
    GlobalState Error.NoError Error.NoError ->
      System.Exit.exitSuccess

runPrompt :: MonadIO m => ReaderT Global m ()
runPrompt = forever $ do
  liftIO $ B.putStr "> " >> hFlush stdout
  line <- liftIO B.getLine
  if B.null line
    then liftIO $ B.putStr "\n"
    else do
      run line
      ask >>= \ref ->
        liftIO $ IORef.writeIORef ref.unGlobal (GlobalState Error.NoError Error.NoError)

run :: MonadIO m => ByteString -> ReaderT Global m ()
run sourceBS = do
  (tokens, err1) <- liftIO $ scanTokens sourceBS
  -- liftIO $ traverse_ print tokens
  (maybeStmts, err2) <- liftIO $ runParse tokens
  case err1 <> err2 of
    Error.Error ->
      ask >>= \ref -> liftIO $
        IORef.modifyIORef' ref.unGlobal $
          \old -> old {hadError = Error.Error}
    Error.NoError ->
      case maybeStmts of
        Nothing -> do
          -- lift . liftIO $ B.putStrLn "no parse success"
          pure ()
        Just stmts -> do
          -- lift . liftIO $ B.putStrLn "parse success"
          liftIO ((fmap runOptimizer) <$> (runResolver stmts)) >>= \case
            Nothing ->
              ask >>= \ref -> liftIO $
                IORef.modifyIORef' ref.unGlobal $
                  \old -> old {hadError = Error.Error}
            Just stmts2 -> do
              hadRuntimeError <- liftIO $ interpret stmts2
              when (hadRuntimeError == Error.Error) $
                ask >>= \ref -> liftIO $ IORef.modifyIORef' ref.unGlobal $ \old -> old {hadRuntimeError = Error.Error}
