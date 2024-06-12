module Main (main) where

import Control.Monad.ST
import Data.ByteString.Char8 as B
import Data.Foldable as F
import Data.Vector as V
import Expr
import FlatParse.Stateful
import Parser qualified as P
import Scanner qualified as S
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Token

main :: IO ()
main = defaultMain tests

success :: Assertion
success = pure ()

-- Scanning ByteString
parseNumberProp :: Property
parseNumberProp = property $ do
  (d, r) <- doubleGen
  let a = case r of
        OK (NUMBER res _) _ _ ->
          let parseErrorRangeProperty
                | d == res = property True
                | (d - 0.0001 <= res) || (res <= d + 0.0001) = property Discard
                | otherwise = property False
           in counterexample
                ("Parsed with error greater than +/- 0.0001: " <> show res <> " /= " <> show d)
                parseErrorRangeProperty
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
            ("Failed with parsing error '" <> show e <> "'")
            (property False)
  pure a -- label ("Parse '" <> show d <> "'") a
  where
    doubleGen :: Gen (Double, Result S.ScannerError Token)
    doubleGen = do
      d <- choose (0, 1500)
      let bsDouble = B.pack $ show d
      let r = runST $ S.simpleScanParserS bsDouble S.parseNumber
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
        ( (badPosTok <$>)
            <$> [ ("(", LEFT_PAREN)
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
        )
    tokPairBSVecGen :: Gen (ByteString, Vector Token)
    tokPairBSVecGen = do
      l <- vectorOf 100 tokPairGen
      let f (prevBS, prevVec) (nextBS, nextTok) = (prevBS <> " " <> nextBS, V.snoc prevVec nextTok)
      pure $ F.foldl' f (B.empty, V.empty) l

    simpleTokGen
      :: Gen (ByteString, Vector Token, Result S.ScannerError (Vector Token))
    simpleTokGen = do
      (bs, tokVec) <- tokPairBSVecGen
      let r = runST $ S.simpleScanParser bs (S.tryUntilEOF S.simpleScanToken)
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
    , testCase "12345 @?= parseNumber" $
        let double = 12345
            doubleBS = B.pack $ show double
            r = runST $ S.simpleScanParserS doubleBS S.parseNumber
         in case r of
              OK (NUMBER res _) _ _ -> double @?= res
              OK otherTok _ _ -> do
                assertFailure $
                  "Incorrect Token: " <> show otherTok <> " Expected: NUMBER " <> show double
              Fail -> do
                assertFailure "Fail"
              Err e -> do
                assertFailure $ "Error: " <> show e
    , testCase "12345. parseNumber fail" $
        let doubleBS = "12345."
            r = runST $ S.simpleScanParserS doubleBS S.parseNumber
         in case r of
              OK anyTok _ _ -> do
                assertFailure $
                  "Should not parse Token: " <> show anyTok <> " expected parsing failure"
              Fail -> do
                assertFailure "Uncorrectly failed parser"
              Err e -> do
                case e of
                  S.InvalidNumberLiteral {} -> success
                  _ ->
                    assertFailure $
                      "Unexpected Error: '" <> show e <> "' expected InvalidNumberLiteral error"
    , testCase "12345.a parseNumber fail" $
        let doubleBS = "12345.a"
            r = runST $ S.simpleScanParserS doubleBS S.parseNumber
         in case r of
              OK anyTok _ _ -> do
                assertFailure $
                  "Should not parse Token: " <> show anyTok <> " expected parsing failure"
              Fail -> do
                assertFailure "Uncorrectly failed parser"
              Err e -> do
                case e of
                  S.InvalidNumberLiteral {} -> success
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
            ( "Parsed incorrect Token Vector: \n'"
                <> show resultVec
                <> "'\n Correct Token Vector: \n'"
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
        ( (badPosTok <$>)
            <$> [ ("and", AND)
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
      l <- vectorOf 5 tokPairGen
      let f (prevBS, prevVec) (nextBS, nextTok) = (prevBS <> " " <> nextBS, V.snoc prevVec nextTok)
      pure $ F.foldl' f (B.empty, V.empty) l

    simpleTokGen
      :: Gen (ByteString, Vector Token, Result S.ScannerError (Vector Token))
    simpleTokGen = do
      (bs, tokVec) <- tokPairBSVecGen
      let r =
            runST $
              S.simpleScanParser
                bs
                (S.tryUntilEOF (S.skipWhiteSpace >> S.parseKeywAndIdentif))
      pure (bs, tokVec, r)

parseKeywAndIdentifTests :: TestTree
parseKeywAndIdentifTests =
  testGroup
    "Test 'parseKeywAndIdentif'"
    [ testProperty
        "Parse a generated (Vector Token) from Token Keywords"
        parseKeyAndIdentifProp
    ]

-- Parsing Tokens

parseTokensTests :: TestTree
parseTokensTests =
  testGroup
    "Test 'many parsePrimary'"
    [ testCase "Parse [FALSE, TRUE, NIL, STRING \"aa\", NUMBER 56.0]'" $ do
        let toks = V.fromList $ badPosTok <$> [FALSE, TRUE, NIL, STRING "aa", NUMBER 56.0]
            correctRes =
              V.fromList
                [ PBoolConstExpr False
                , PBoolConstExpr True
                , PNilExpr
                , PStrExpr "aa"
                , PNumberExpr 56.0
                ]
            r = P.runParser (many P.parsePrimary) toks
        case r of
          P.OK res _ -> correctRes @?= V.fromList res
          P.Fail -> do
            assertFailure "Fail"
          P.Err (e :: ()) -> do
            assertFailure $ "Error: " <> show e
    ]

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "ByteString Scanning tests"
        [ parseSimpleTokenTests
        , parseNumberTests
        , parseKeywAndIdentifTests
        ]
    , testGroup
        "Token Parsing tests"
        [parseTokensTests]
    ]
