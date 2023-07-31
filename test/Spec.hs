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

simpleDouble :: Gen (Maybe Property)
simpleDouble = do
  double :: Double <- traceShowId <$> (choose (-1000, 1000) :: Gen Double)
  let bsDouble = B.pack $ show double
      r = runST $ do
        stref <- newSTRef $ ScanErr V.empty
        rr <- runParserST parseNumber stref 0 bsDouble
        pure $ case rr of
          OK (NUMBER res) _ _ -> Just $ res === double
          _ -> Nothing
  pure r

success :: Assertion
success = pure ()

showErrorsCollected :: Show a => STRef RealWorld a -> IO ()
showErrorsCollected ref = do
  errs <- stToIO $ readSTRef ref
  Prelude.putStrLn $ "Parser collected failures: \n" <> show errs

tests =
  testGroup
    "parseNumber"
    [ testProperty "simpleDouble" simpleDouble
    , testCase "12345 @?= parseNumber" $ do
        stref <- stToIO . newSTRef $ ScanErr V.empty
        let double = 12345
        r <- stToIO $ runParserST parseNumber stref 0 (B.pack $ show double)
        case r of
          OK (NUMBER res) _ _ -> double @?= res
          OK otherTok _ _ -> do
            showErrorsCollected stref
            assertFailure $
              "Incorrect Token: " <> show otherTok <> " Expected: NUMBER " <> show double
          Fail -> do
            showErrorsCollected stref
            assertFailure "Fail"
          Err e -> do
            showErrorsCollected stref
            assertFailure $ "Error: " <> show e
    , testCase "12345. parseNumber" $ do
        stref <- stToIO . newSTRef $ ScanErr V.empty
        let doubleStr = "12345."
        r <- stToIO $ runParserST parseNumber stref 0 doubleStr
        case r of
          OK anyTok _ _ -> do
            showErrorsCollected stref
            assertFailure $
              "Should not parse Token: " <> show anyTok <> " expected parsing failure "
          Fail -> do
            B.putStrLn "Correctly failed parser"
            showErrorsCollected stref
            success
          Err e -> do
            showErrorsCollected stref
            assertFailure $ "Unexpected Error: '" <> show e <> "' expected parsing failure"
    ]
