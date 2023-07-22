module Main (main) where

import CmdlineOptions
import Control.Monad.State.Strict
import Data.ByteString.Char8 qualified as B
import Data.Function ((&))
import Scanner
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import Data.Vector qualified as V
import Token
import Data.ByteString.Char8 (ByteString)
import Data.Vector (Vector)

newtype AppState = StateErr Bool

-- Define a monad stack using MTL
newtype AppM a
  = AppM
      ( StateT AppState IO a
      )
  deriving (Functor, Applicative, Monad, MonadState AppState, MonadIO)

runAppM :: AppM a -> IO a
runAppM (AppM x) = evalStateT x initialState
  where
    initialState = StateErr False

main :: IO ()
main = do
  (Options sourceCodeFilepath) <- execParser options
  case sourceCodeFilepath of
    Nothing -> runAppM runPrompt
    Just sourceCodeFile -> runAppM $ runFile sourceCodeFile

runFile :: (MonadIO m, MonadState AppState m) => ByteString -> m ()
runFile path = do
  liftIO $ B.putStrLn ("Run file: " <> path)
  fileContents <- liftIO $ B.readFile (B.unpack path)
  run fileContents
  (StateErr err) <- get
  when err (liftIO $ exitWith (ExitFailure 65))

runPrompt :: MonadIO m => m ()
runPrompt = forever $ do
  liftIO $ B.putStr "> "
  line <- liftIO B.getLine
  liftIO $
    if B.null line
      then B.putStr "\n"
      else run line

run :: MonadIO m => ByteString -> m ()
run source = do
  tokens <- scan source
  liftIO $ printRes tokens
  where
        printRes toks = case toks of
          Right (v, bs) -> let p = B.putStrLn . B.pack . show in 
            do 
              forM_ v p
              B.putStrLn ("Rest of BS: " <> bs)
          Left (ERR ch (line, col))-> 
            let str = "UnexpectedCharacter '" <> B.singleton ch 
                    <> "' at l:" <> B.pack (show $ line+1)
                    <> ", c:" <> B.pack (show $ col +1)
            in B.putStrLn str

scan :: MonadIO m => ByteString -> m (Either CodeError (Vector Token, ByteString))
scan b = liftIO $ scanFile b

reportError :: (MonadIO m, MonadState AppState m) => Int -> ByteString -> m ()
reportError lineNum message = do
  liftIO $
    B.hPutStrLn stderr $
      "[line " <> (lineNum & show & B.pack) <> "] Error: " <> message
  put (StateErr True)
