module Main (main) where

import Control.Monad.ST
import Data.ByteString.Char8 as B
import Data.STRef
import Data.Vector as V
import Debug.Trace (traceShow, traceShowId)
import FlatParse.Stateful
import Scanner
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Token (Token (NUMBER))

main :: IO ()
main = defaultMain tests

success :: Assertion
success = pure ()

parseNumberProp :: Property
parseNumberProp = property $ do
  (d, r) <- doubleGen
  let a = case r of
        OK (NUMBER res) _ _ -> res === d
        OK otherTok _ _ ->
          counterexample
            ("Parsed incorrect token '" <> show otherTok <> "' failed")
            (property False)
        Fail ->
          counterexample
            ("Parsing of '" <> show d <> "' failed")
            (property False)
        Err e ->
          counterexample
            ("Pailed with parsing error '" <> show e <> "'")
            (property False)
  pure $ label ("Parse '" <> show d <> "'") a
  where
    doubleGen :: Gen (Double, Result ScannerError Token)
    doubleGen = do
      d <- choose (-1000, 1000)
      let bsDouble = B.pack $ show d
      let r = runST $ do
            stref <- newSTRef $ ScanErr V.empty
            runParserST parseNumber stref 0 bsDouble

      pure (d, r)

showErrorsCollected :: ByteString -> STRef RealWorld ScanErr -> IO ()
showErrorsCollected bs ref = do
  (ScanErr v) <- stToIO $ readSTRef ref
  B.putStrLn "Parser collected failures: "
  printErrs bs v

tests :: TestTree
tests =
  testGroup
    "Test 'parseNumber'"
    [ testProperty "Parse (NUMBER Double) from 'choose (-1000, 1000)'" parseNumberProp
    , testCase "12345 @?= parseNumber" $ do
        stref <- stToIO . newSTRef $ ScanErr V.empty
        let double = 12345
            doubleBS = B.pack $ show double
        r <- stToIO $ runParserST parseNumber stref 0 doubleBS
        case r of
          OK (NUMBER res) _ _ -> double @?= res
          OK otherTok _ _ -> do
            assertFailure $
              "Incorrect Token: " <> show otherTok <> " Expected: NUMBER " <> show double
          Fail -> do
            assertFailure "Fail"
          Err e -> do
            assertFailure $ "Error: " <> show e
    , testCase "12345. parseNumber fail" $ do
        stref <- stToIO . newSTRef $ ScanErr V.empty
        let doubleBS = "12345."
        r <- stToIO $ runParserST parseNumber stref 0 doubleBS
        case r of
          OK anyTok _ _ -> do
            assertFailure $
              "Should not parse Token: " <> show anyTok <> " expected parsing failure"
          Fail -> do
            assertFailure "Uncorrectly failed parser"
          Err e -> do
            case e of
              InvalidNumberLiteral _ _ errBs -> do
                success
              _ ->
                assertFailure $
                  "Unexpected Error: '" <> show e <> "' expected InvalidNumberLiteral error"
    , testCase "12345.a parseNumber fail" $ do
        stref <- stToIO . newSTRef $ ScanErr V.empty
        let doubleBS = "12345.a"
        r <- stToIO $ runParserST parseNumber stref 0 doubleBS
        case r of
          OK anyTok _ _ -> do
            assertFailure $
              "Should not parse Token: " <> show anyTok <> " expected parsing failure"
          Fail -> do
            -- showErrorsCollected doubleBS stref
            assertFailure "Uncorrectly failed parser"
          Err e -> do
            case e of
              InvalidNumberLiteral _ _ errBs -> do
                success
              _ ->
                assertFailure $
                  "Unexpected Error: '" <> show e <> "' expected InvalidNumberLiteral error"
    ]
