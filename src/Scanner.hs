{-# LANGUAGE OverloadedRecordDot #-}

module Scanner where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS.CPS (RWST)
import Control.Monad.Reader
  ( MonadReader (ask)
  , MonadTrans (lift)
  , ReaderT (runReaderT)
  )
import Control.Monad.ST qualified as ST
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef qualified as IORef
import Data.STRef qualified as STRef
import TokenType (Token (..), TokenType (..))

data Scanner = Scanner
  { source :: ByteString
  , tokens :: [Token]
  , start :: Int
  , current :: Int
  , line :: Int
  }

type ScanM a = forall s. ReaderT (STRef.STRef s Scanner) (ST.ST s) a

type Errors = [String]

scanTokens :: ByteString -> [Token]
scanTokens source = ST.runST $ do
  ref <- STRef.newSTRef initialScanner
  flip runReaderT ref $ do
    whileM
      (not <$> isAtEnd)
      ( do
          lift $
            STRef.modifySTRef' ref $
              \scanner -> scanner {start = scanner.current}
          scanToken
      )
    lift $
      STRef.modifySTRef' ref $
        \scanner ->
          scanner
            { tokens =
                Token {ttype = EOF, lexeme = "", literal = Nothing, tline = scanner.line}
                  : scanner.tokens
            }

    lift $ STRef.readSTRef ref <&> (.tokens)
  where
    initialScanner =
      Scanner
        { source = source
        , tokens = []
        , start = 0
        , current = 0
        , line = 1
        }
    isAtEnd :: ScanM Bool
    isAtEnd = do
      scanner <- ask >>= \v -> lift (STRef.readSTRef v)
      pure $ scanner.current >= BS.length scanner.source
    scanToken :: ScanM ()
    scanToken = do
      c <- advance
      case c of
        '(' -> addToken1 LEFT_PAREN
        ')' -> addToken1 RIGHT_PAREN
        '{' -> addToken1 LEFT_BRACE
        '}' -> addToken1 RIGHT_BRACE
        ',' -> addToken1 COMMA
        '.' -> addToken1 DOT
        '-' -> addToken1 MINUS
        '+' -> addToken1 PLUS
        ';' -> addToken1 SEMICOLON
        '*' -> addToken1 STAR
        _ -> lerror sc.line "Unexcpected character"
    advance :: ScanM Char
    advance = do
      scanner <- ask >>= lift . STRef.readSTRef
      let newScanner = scanner {current = scanner.current + 1}
      ask >>= \ref -> lift $ STRef.writeSTRef ref newScanner
      pure $ BS.index newScanner.source newScanner.current
    addToken1 :: TokenType -> ScanM ()
    addToken1 ttype = addToken2 ttype Nothing
    addToken2 :: TokenType -> Maybe String -> ScanM ()
    addToken2 ttype literal = do
      ask >>= \ref -> lift
        $ STRef.modifySTRef'
          ref
        $ \sc ->
          let text = BS.take (sc.current - sc.start) (BS.drop sc.start sc.source)
           in sc {tokens = Token ttype text literal sc.line : sc.tokens}

whileM :: Monad m => m Bool -> m a -> m ()
whileM cond act = do
  r <- cond
  when r $ whileM cond act

lerror :: MonadIO m => Int -> String -> m ()
lerror line msg = report line "" msg

report :: MonadIO m => Int -> String -> String -> m ()
report line location msg = liftIO $ do
  putStrLn $ mconcat ["[line ", show line, "] Error", location, ": ", msg]
  ask >>= \ref -> IORef.writeIORef ref True
