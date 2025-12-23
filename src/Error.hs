{-# LANGUAGE OverloadedRecordDot #-}

module Error
  ( scanError
  , parseError
  , reportRuntimeError
  , ErrorPresent (..)
  , RuntimeException (..)
  , resolverError
  )
where

import Bluefin.Eff
import Bluefin.IO (IOE, effIO)
import Bluefin.Writer (Writer, tell)
import Data.ByteString.Char8 as BS
import Data.ByteString.Short qualified as SBS
import {-# SOURCE #-} Expr qualified
import System.IO (stderr)
import TokenType (Token (..), TokenType (..))

data ErrorPresent
  = NoError
  | Error
  deriving (Show, Eq)

-- Behaves like Data.Monoid.Any
instance Semigroup ErrorPresent where
  Error <> _ = Error
  _ <> Error = Error
  _ <> _ = NoError

instance Monoid ErrorPresent where
  mempty = NoError

scanError
  :: (io :> es, writer :> es)
  => IOE io
  -> Writer ErrorPresent writer
  -> Int
  -> ByteString
  -> Eff es ()
scanError io w line = report io w line ""

parseError
  :: (io :> es, writer :> es)
  => IOE io
  -> Writer ErrorPresent writer
  -> Token
  -> ByteString
  -> Eff es ()
parseError io w token message =
  if token.ttype == EOF
    then report io w token.tline " at end" message
    else
      report io w token.tline (" at '" <> SBS.fromShort token.lexeme <> "'") message

resolverError
  :: (io :> es, writer :> es)
  => IOE io
  -> Writer ErrorPresent writer
  -> Token
  -> ByteString
  -> Eff es ()
resolverError = parseError

data RuntimeException
  = RuntimeError {line :: !Int, message :: !ByteString}
  | RuntimeReturn {value :: !Expr.LiteralValue}

reportRuntimeError
  :: io :> es => IOE io -> RuntimeException -> Eff es ()
reportRuntimeError io rerror =
  effIO io $
    putStrLnStderr $
      rerror.message <> "\n[line " <> BS.pack (show rerror.line) <> "]"

report
  :: (io :> es, writer :> es)
  => IOE io
  -> Writer ErrorPresent writer
  -> Int
  -> ByteString
  -> ByteString
  -> Eff es ()
report io w line whereLocation message = do
  let er =
        mconcat
          ["[line ", BS.pack $ show line, "] Error", whereLocation, ": ", message]
  effIO io $ putStrLnStderr er
  tell w Error

putStrLnStderr :: ByteString -> IO ()
putStrLnStderr = BS.hPutStrLn stderr
