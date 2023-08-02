module Main (main) where

import Control.Monad.ST
import Data.ByteString.Char8 as B
import Data.Foldable as F
import Data.STRef
import Data.Vector as V
import Debug.Trace (traceShow, traceShowId)
import FlatParse.Stateful
import Scanner
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Token (Token (..))

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
  pure a -- label ("Parse '" <> show d <> "'") a
  where
    doubleGen :: Gen (Double, Result ScannerError Token)
    doubleGen = do
      d <- choose (0, 1500)
      let bsDouble = B.pack $ show d
      let r = runST $ do
            stref <- newSTRef $ ScanErr V.empty
            runParserST parseNumber stref 0 bsDouble

      pure (d, r)

parseSimpleTokProp :: Property
parseSimpleTokProp = property $ do
  (bs, v, r) <- simpleTokGen
  let a = case r of
        OK resultVec _ _ ->
          -- resultVec === v
          counterexample
            ( "Parsed incorrect token Vector \n'"
                <> show resultVec
                <> "'\n and correctT token Vector \n'"
                <> show v
                <> "'\n ORIGINAL BS:\n"
                <> show bs
            )
            (property $ resultVec == v)
        Fail ->
          counterexample
            ("Parsing of '" <> show bs <> "' failed")
            (property False)
        Err e ->
          counterexample
            ("Pailed with parsing error '" <> show e <> "'")
            (property False)
  pure a
  where
    tokPairGen :: Gen (ByteString, Token)
    tokPairGen =
      elements
        [ ("(", LEFT_PAREN)
        , (")", RIGHT_PAREN)
        , ("{", LEFT_BRACE)
        , ("}", RIGHT_BRACE)
        , (",", COMMA)
        , (".", DOT)
        , ("-", MINUS)
        , ("+", PLUS)
        , (";", SEMICOLON)
        , ("*", STAR)
        , ("!", BANG)
        , ("!=", BANG_EQUAL)
        , ("=", EQUAL)
        , ("==", EQUAL_EQUAL)
        , ("<", LESS)
        , ("<=", LESS_EQUAL)
        , (">", GREATER)
        , (">=", GREATER_EQUAL)
        , ("/", SLASH)
        ]

    tokPairBSVecGen :: Gen (ByteString, Vector Token)
    tokPairBSVecGen = do
      l <- vectorOf 100 tokPairGen
      let f (prevBS, prevVec) (nextBS, nextTok) = (prevBS <> " " <> nextBS, V.snoc prevVec nextTok)
      pure $ F.foldl' f (B.empty, V.empty) l

    simpleTokGen
      :: Gen (ByteString, Vector Token, Result ScannerError (Vector Token))
    simpleTokGen = do
      (bs, tokVec) <- tokPairBSVecGen
      let r = runST $ do
            stref <- newSTRef $ ScanErr V.empty
            runParserST (tryUntilEOF simpleScanToken) stref 0 bs
      pure (bs, tokVec, r)

parseSimpleTokenTests :: TestTree
parseSimpleTokenTests =
  testGroup
    "Test 'simpleScanToken'"
    [testProperty "Parse a generated (Vector Token)" parseSimpleTokProp]

parseNumberTests :: TestTree
parseNumberTests =
  testGroup
    "Test 'parseNumber'"
    [ testProperty "Parse (NUMBER Double) from 'choose (0, 1500)'" parseNumberProp
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
              InvalidNumberLiteral {} -> success
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
            assertFailure "Uncorrectly failed parser"
          Err e -> do
            case e of
              InvalidNumberLiteral {} -> success
              _ ->
                assertFailure $
                  "Unexpected Error: '" <> show e <> "' expected InvalidNumberLiteral error"
    ]

parseKeyAndIdentifProp :: Property
parseKeyAndIdentifProp = property $ do
  (bs, v, r) <- simpleTokGen
  let a = case r of
        OK resultVec _ _ ->
          -- resultVec === v
          counterexample
            ( "Parsed incorrect token Vector \n'"
                <> show resultVec
                <> "'\n and correctT token Vector \n'"
                <> show v
                <> "'\n ORIGINAL BS:\n"
                <> show bs
            )
            (property $ resultVec == v)
        Fail ->
          counterexample
            ("Parsing of '" <> show bs <> "' failed")
            (property False)
        Err e ->
          counterexample
            ("Pailed with parsing error '" <> show e <> "'")
            (property False)
  pure a
  where
    tokPairGen :: Gen (ByteString, Token)
    tokPairGen = do
      v <-
        vectorOf 10 gen
      elements
        ( [ ("and", AND)
          , ("class", CLASS)
          , ("else", ELSE)
          , ("false", FALSE)
          , ("for", FOR)
          , ("fun", FUNN)
          , ("if", IF)
          , ("nil", NIL)
          , ("or", OR)
          , ("print", PRINT)
          , ("return", RETURN)
          , ("super", SUPER)
          , ("this", THIS)
          , ("true", TRUE)
          , ("var", VAR)
          , ("while", WHILE)
          ]
            <> v
        )
      where
        gen = do
          x <- suchThat (chooseEnum ('A', 'z')) isLatinLetter
          xs <-
            vectorOf 6 $
              suchThat (chooseEnum ('0', 'z')) (\c -> isDigit c || isLatinLetter c)
          pure (B.pack (x : xs), IDENTIFIER)

    tokPairBSVecGen :: Gen (ByteString, Vector Token)
    tokPairBSVecGen = do
      l <- vectorOf 20 tokPairGen
      let f (prevBS, prevVec) (nextBS, nextTok) = (prevBS <> " " <> nextBS, V.snoc prevVec nextTok)
      pure $ F.foldl' f (B.empty, V.empty) l

    simpleTokGen
      :: Gen (ByteString, Vector Token, Result ScannerError (Vector Token))
    simpleTokGen = do
      (bs, tokVec) <- tokPairBSVecGen
      let r = runST $ do
            stref <- newSTRef $ ScanErr V.empty
            runParserST (tryUntilEOF (skipWhiteSpace >> parseKeywAndIdentif)) stref 0 bs
      pure (bs, tokVec, r)

parseKeywAndIdentifTests :: TestTree
parseKeywAndIdentifTests =
  testGroup
    "parseKeywAndIdentifTests"
    [ testProperty
        "Parse a generated (Vector Token) from Token Keywords"
        parseKeyAndIdentifProp
    ]

tests :: TestTree
tests =
  testGroup
    "Parsing tests"
    [ parseSimpleTokenTests
    , parseNumberTests
    , parseKeywAndIdentifTests
    ]
