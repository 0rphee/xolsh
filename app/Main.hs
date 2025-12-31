module Main (main) where

import Bluefin.Eff (runEff)
import Bluefin.State (evalState)
import CmdlineOptions qualified
import Error qualified
import Run qualified

main :: IO ()
main = do
  (CmdlineOptions.Options sourceCodeFilepath) <-
    CmdlineOptions.execParser CmdlineOptions.options
  runEff $ \io -> evalState initialState $
    \ge ->
      case sourceCodeFilepath of
        Nothing -> Run.runPrompt io ge
        Just sourceCodeFile -> Run.runFile io ge sourceCodeFile
  where
    initialState =
      (Run.GlobalState {hadError = Error.NoError, hadRuntimeError = Error.NoError})
