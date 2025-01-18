{-# LANGUAGE QuasiQuotes #-}

module Programs (lightTests, heavyTests) where

import Data.ByteString.Char8 (ByteString)
import Data.String.Interpolate (iii)

lightTests :: [(String, ByteString)]
lightTests =
  [ t_concatAndMult
  , t_inherited_method
  , t_assign_to_closure
  , t_recfib5
  , t_nested_closure
  ]

heavyTests :: [(String, ByteString)]
heavyTests = [t_recfib27]

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

t_recfib5 :: (String, ByteString)
t_recfib5 =
  ("rec fib(5)",) $
    [iii|
    fun fib(n) {
      if (n <= 1) return n;
      return fib(n - 2) + fib(n - 1);
    }
    fib(5);
    |]

t_inherited_method :: (String, ByteString)
t_inherited_method =
  ("inherited_method",) $
    [iii|
    class Foo { inFoo() { print "in foo"; } }
    class Bar < Foo { inBar() { print "in bar"; } }
    class Baz < Bar { inBaz() { print "in baz"; } }
    var baz = Baz();
    baz.inFoo(); baz.inBar(); baz.inBaz();
    |]

t_assign_to_closure :: (String, ByteString)
t_assign_to_closure =
  ("assign_to_closure",) $
    [iii|
    var f; var g;
    {
      var local = "local";
      fun f_() { print local; local = "after f"; print local; }
      f = f_;
      fun g_() { print local; local = "after g"; print local; }
      g = g_;
    }
    f(); g();
    |]

t_nested_closure :: (String, ByteString)
t_nested_closure =
  ("nested_closure",) $
    [iii|
    var f;
    fun f1() {
      var a = "a";
      fun f2() {
        var b = "b";
        fun f3() {
          var c = "c";
          fun f4() { print a; print b; print c; }
          f = f4;
        }
        f3();
      }
      f2();
    }
    f1();
    f();
    |]
