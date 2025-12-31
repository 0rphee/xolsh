{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Run (run, runFile, runPrompt, GlobalState (..)) where

import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE, effIO)
import Bluefin.State (State)
import Bluefin.State qualified as State
import Bluefin.Writer (runWriter)
import Control.Monad (forever, when)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Error qualified
import Interpreter (interpret)
import Parser (runParse)
import Resolver (runResolver)
import Scanner (scanTokens)
import System.Exit qualified
import System.IO qualified

data GlobalState
  = GlobalState
  { hadError :: !Error.ErrorPresent,
    hadRuntimeError :: !Error.ErrorPresent
  }

run ::
  (io :> es, ge :> es) =>
  IOE io -> State GlobalState ge -> ByteString -> Eff es ()
run io ge sourceBS =
  ( runWriter $ \w -> do
      tokens <- scanTokens io w sourceBS
      -- liftIO $ traverse_ print tokens
      runParse io w tokens
  )
    >>= \case
      (_, Error.Error) ->
        State.modify ge $ \s -> s {hadError = Error.Error}
      (stmts, Error.NoError) -> do
        -- lift . liftIO $ B.putStrLn "parse success"
        runWriter (\w -> runResolver io w stmts) >>= \case
          (_, Error.Error) ->
            State.modify ge $ \s -> s {hadError = Error.Error}
          (stmts2, Error.NoError) -> do
            hadRuntimeError <- interpret io stmts2
            when (hadRuntimeError == Error.Error) $ State.modify ge $ \s -> s {hadRuntimeError = Error.Error}

runFile ::
  (io :> es, ge :> es) =>
  IOE io -> State GlobalState ge -> ByteString -> Eff es ()
runFile io ge path = do
  -- liftIO $ B.putStrLn ("Run file: " <> path)
  fileContents <- effIO io $ B.readFile (B.unpack path)
  run io ge fileContents
  globalState <- State.get ge
  effIO io $ case globalState of
    GlobalState Error.Error _ ->
      System.Exit.exitWith $ System.Exit.ExitFailure 65
    GlobalState Error.NoError Error.Error ->
      System.Exit.exitWith $ System.Exit.ExitFailure 70
    GlobalState Error.NoError Error.NoError ->
      System.Exit.exitSuccess

runPrompt ::
  (io :> es, ge :> es) => IOE io -> State GlobalState ge -> Eff es ()
runPrompt io ge = forever $ do
  effIO io $ B.putStr "> " >> System.IO.hFlush System.IO.stdout
  line <- effIO io B.getLine
  if B.null line
    then effIO io $ B.putStr "\n"
    else do
      run io ge line
      State.put ge $ GlobalState Error.NoError Error.NoError
