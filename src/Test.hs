module Test where

import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Foldable
import Data.Vector (Vector)
import Data.Vector qualified as V
import Diagnostics
import FlatParse.Stateful
import Scanner
import Token

testGod :: ByteString -> IO ()
testGod txt =
  case runST $
    simpleScanParser txt (untilEnd scanSingleToken) of
    (evec, OK (res :: Vector _) _ bs) -> do
      traverse_ print res
      -- traverse_ print $ posLineCols txt $ V.toList res
      printErrors "shell" txt evec
      putStrLn $ "Rest of BS: '" <> B.unpack bs <> "'"
    (evec, Err e) -> do
      putStrLn $ "Error: --" <> show e <> "--"
      printErrors "shell" txt evec
    (evec, Fail) -> do
      putStrLn "Unhandled failure'"
      printErrors "shell" txt evec
