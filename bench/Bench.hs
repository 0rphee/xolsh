{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.String.Interpolate (iii)
import Error qualified
import Interpreter qualified
import Parser qualified
import Resolver qualified
import Scanner qualified
import System.IO.Silently
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ bgroup
        "interpretStr benchmarks"
        [ bench (name) $ whnfIO $ interpretStr bs
        | ((strname, bs)) <- testStrings
        , let name = if B.length bs > 20 then strname else B.unpack bs
        ]
    ]
  where
    testStrings = [t_concatAndMult, t_recfib27, t_inherited_method]

t_concatAndMult :: (String, ByteString)
t_concatAndMult =
  ("Str concat and num mult in for loop",) $
    [iii|
    var a;  var num = 0;
    for (var i = 0; i < 5; i = i + 1 ) {
      a = "str" + "str" + "str";
      num = num * i;
    }
    |]

t_recfib27 :: (String, ByteString)
t_recfib27 =
  ("rec fib(27)",) $
    [iii|
    fun fib(n) {
      if (n <= 1) return n;
      return fib(n - 2) + fib(n - 1);
    }
    fib(27);
    |]

t_inherited_method :: (String, ByteString)
t_inherited_method =
  ("inherited method",) $
    [iii|
    class Foo { inFoo() { print "in foo"; } }
    class Bar < Foo { inBar() { print "in bar"; } }
    class Baz < Bar { inBaz() { print "in baz"; } }
    var baz = Baz();
    baz.inFoo(); baz.inBar(); baz.inBaz();
    |]

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
