{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Bluefin.Eff (runEff)
import Bluefin.Exception qualified as Exception
import Bluefin.IO (effIO)
import Bluefin.Writer (runWriter)
import Control.Monad (void)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Functor ((<&>))
import Data.Vector qualified as V
import Error qualified
import Instances.TH.Lift ()
import Interpreter qualified
import Language.Haskell.TH.Syntax qualified as TH
import Optimizer qualified
import Parser qualified
import Programs qualified
import Resolver qualified
import Scanner qualified
import Stmt qualified
import System.IO.Silently qualified as Silently
import System.Process qualified as P
import Test.Tasty (localOption)
import Test.Tasty.Bench
  ( Benchmark
  , TimeMode (WallTime)
  , bench
  , bgroup
  , defaultMain
  , nfAppIO
  , whnfIO
  )
import TokenType qualified

main :: IO ()
main = do
  let cmd = "cabal build exe:xolsh-exe"
  putStrLn $ "Running: " <> cmd
  P.waitForProcess =<< P.runCommand cmd
  xolshexe <- P.readProcess "cabal" ["list-bin", "exe:xolsh-exe"] "" <&> init
  putStrLn $ "xolsh-exe at: " <> xolshexe
  defaultMain
    [ bgroup
        "interpretStr"
        [ {- bgroup
              "lightTests"
              (benchPrograms Programs.lightTests)
          , -}
          bgroup
            "heavyTests"
            (benchPrograms Programs.heavyTests)
        , bgroup
            "loxLoxTests"
            (benchLoxLoxPrograms xolshexe Programs.loxLoxTests)
        ]
    , bgroup
        "scanning"
        [ bgroup "lightTests" $ benchScanning $(TH.lift Programs.lightTests)
        , bgroup "heavyTests" $ benchScanning $(TH.lift Programs.heavyTests)
        , bgroup "loxlox" []
        ]
    , bgroup
        "parsing"
        [ bgroup "lightTests" $
            benchParsing
              $( (TH.runIO $ Programs.comptimeScanningAction Programs.lightTests)
                  >>= TH.lift
               )
        , bgroup "heavyTests" $
            benchParsing
              $( (TH.runIO $ Programs.comptimeScanningAction Programs.heavyTests)
                  >>= TH.lift
               )
        , bgroup "loxlox" []
        ]
    ]
  where
    -- let finalLoxLoxCmd =
    --       "cat loxexamples/ch10.lox | cabal run xolsh-exe -- loxlox/Lox.lox && eventlog2html xolsh-exe.eventlog && open xolsh-exe.eventlog.html && ll"
    -- putStrLn $ "Running: " <> finalLoxLoxCmd
    -- void $
    --   P.waitForProcess
    --     =<< P.runCommand finalLoxLoxCmd

    benchPrograms :: [(String, ByteString)] -> [Benchmark]
    benchPrograms proglist =
      [ bench (name) $ whnfIO $ interpretStr bs
      | ((strname, bs)) <- proglist
      , let name = if B.length bs > 20 then strname else B.unpack bs
      ]
    benchLoxLoxPrograms :: FilePath -> [FilePath] -> [Benchmark]
    benchLoxLoxPrograms xolshexe proglist =
      [ localOption WallTime $
          bench loxfilepath $
            ( \exe ->
                Silently.silence $
                  void $
                    P.waitForProcess
                      =<< P.runCommand
                        (concat ["sh -c \"cat ", loxfilepath, " | ", exe, " loxlox/Lox.lox\""])
            )
              `nfAppIO` xolshexe
      | loxfilepath <- proglist
      ]

benchScanning :: [(String, ByteString)] -> [Benchmark]
benchScanning proglist =
  [ bench name $ whnfIO $ runScanningIO bs
  | (name, bs) <- proglist
  ]

runScanningIO :: ByteString -> IO (V.Vector TokenType.Token, Error.ErrorPresent)
runScanningIO sourceBS = runEff $ \io -> runWriter $ \w -> Scanner.scanTokens io w sourceBS

benchParsing :: [(String, V.Vector TokenType.Token)] -> [Benchmark]
benchParsing proglist =
  [ bench name $ whnfIO $ runParsingIO bs
  | (name, bs) <- proglist
  ]

runParsingIO
  :: V.Vector TokenType.Token -> IO (V.Vector Stmt.Stmt1, Error.ErrorPresent)
runParsingIO tokens = runEff $ \io -> runWriter $ \w -> Parser.runParse io w tokens

interpretStr :: ByteString -> IO ()
interpretStr sourceBS =
  ( runEff $ \io -> Exception.try $ \ex ->
      do
        (runWriter $ \w -> Scanner.scanTokens io w sourceBS >>= Parser.runParse io w)
        >>= \case
          (_, Error.Error) -> Exception.throw ex "Error while scanning or parsing."
          (stmts, Error.NoError) -> do
            (runWriter $ \w -> (Resolver.runResolver io w stmts)) >>= \case
              (_, Error.Error) -> Exception.throw ex "Error in resolver."
              (stmts', Error.NoError) -> do
                ( effIO io $ Silently.silence $ runEff $ \io' -> Interpreter.interpret io' (Optimizer.runOptimizer stmts')
                  )
                  >>= \case
                    Error.Error -> Exception.throw ex "Error while interpreting."
                    Error.NoError -> pure ()
  )
    >>= \case
      Left e -> fail e
      _ -> pure ()