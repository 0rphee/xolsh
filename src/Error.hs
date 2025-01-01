{-# LANGUAGE OverloadedRecordDot #-}

module Error (scanError, parseError, ErrorPresent (..)) where

import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Data.ByteString.Char8 as BS
import System.IO (stderr)
import TokenType (Token (..), TokenType (..))

data ErrorPresent
  = NoError
  | Error
  deriving (Show)

-- Behaves like Data.Monoid.Any
instance Semigroup ErrorPresent where
  Error <> _ = Error
  _ <> Error = Error
  _ <> _ = NoError

instance Monoid ErrorPresent where
  mempty = NoError

scanError
  :: (MonadIO m, MonadWriter ErrorPresent m) => Int -> ByteString -> m ()
scanError line = report line ""

parseError
  :: (MonadIO m, MonadWriter ErrorPresent m) => Token -> ByteString -> m ()
parseError token message =
  if token.ttype == EOF
    then report token.tline " at end" message
    else report token.tline (" at '" <> token.lexeme <> "'") message

report
  :: (MonadIO m, MonadWriter ErrorPresent m)
  => Int
  -> ByteString
  -> ByteString
  -> m ()
report line whereLocation message = do
  let er =
        mconcat
          ["[line ", BS.pack $ show line, "] Error", whereLocation, ": ", message]
  liftIO $ putStrLnStderr er
  tell Error

putStrLnStderr :: ByteString -> IO ()
putStrLnStderr = BS.hPutStrLn stderr
