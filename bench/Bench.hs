{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Error qualified
import Interpreter qualified
import Parser qualified
import Programs qualified
import Resolver qualified
import Scanner qualified
import System.IO.Silently
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ bgroup
        "interpretStr"
        [ bgroup
            "lightTests"
            (benchPrograms Programs.lightTests)
        , bgroup
            "heavyTests"
            (benchPrograms Programs.heavyTests)
        ]
    ]
  where
    benchPrograms proglist =
      [ bench (name) $ whnfIO $ interpretStr bs
      | ((strname, bs)) <- proglist
      , let name = if B.length bs > 20 then strname else B.unpack bs
      ]

interpretStr :: ByteString -> IO ()
interpretStr sourceBS = do
  (tokens, err1) <- liftIO $ Scanner.scanTokens sourceBS
  (maybeStmts, err2) <- liftIO $ Parser.runParse tokens
  case err1 <> err2 of
    Error.Error ->
      fail "Error while scanning or parsing."
    Error.NoError ->
      case maybeStmts of
        Nothing -> fail "Error while scanning or parsing."
        Just stmts -> do
          liftIO (Resolver.runResolver stmts) >>= \case
            Nothing -> fail "Error in resolver."
            Just stmts2 -> do
              (silence $ Interpreter.interpret stmts2) >>= \case
                Error.Error -> fail "Error while interpreting."
                Error.NoError -> pure ()
