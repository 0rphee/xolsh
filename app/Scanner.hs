{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module Scanner where

import Control.Monad.Writer.Strict
import Data.ByteString.Char8 qualified as B
import Data.Vector qualified as V
import FlatParse.Stateful
import Token
import Data.ByteString.Char8 (ByteString)
import Data.Vector (Vector)
import FlatParse.Common.Parser (IOMode)
import Control.Concurrent

data ScannerError = UnexpectedCharacter Char Pos deriving (Show)

data CodeError = ERR Char (Int, Int) deriving (Show)

newtype ScanErr = ScanErr (Vector ScannerError)
  deriving Show

data ScannerState = ScannerState
  { source :: !ByteString
  , tokens :: !(Vector Token)
  }

-- newtype Scanner st err parsingRes = Scanner {getScanner :: ParserT st (MVar ScanErr) err parsingRes}
--   deriving (Functor, Applicative, Monad)

printErrs :: ByteString -> Vector ScannerError -> IO ()
printErrs bs vec =
  if V.null vec
  then print "No Errors!"
  else mapM_ ppPrintErrs finalLi
    where (charVec, posVec) = let f' (UnexpectedCharacter ch pos) = (ch, pos)
                in  V.unzip $ fmap f' vec
          linecols :: [(Int, Int)]
          linecols = posLineCols bs $ V.toList posVec

          finalLi :: [(Char, Int, Int)]
          finalLi = let charList = V.toList charVec
                        f char (line, col) = (char,line,col)
                    in zipWith f charList linecols

          ppPrintErrs = p
            
p (ch, line, col)= 
  let str = "UnexpectedCharacter '" <> B.singleton ch 
          <> "' at l:" <> B.pack (show $ line+1)
          <> ", c:" <> B.pack (show $ col +1)
  in B.putStrLn str


scanFile :: ByteString -> IO (Either CodeError (Vector Token, ByteString))
scanFile bs = do
  mvar <- newMVar $ ScanErr V.empty

  res <- runParserIO scanTokens mvar 0 bs

  (ScanErr errVec) <- readMVar mvar

  print errVec
  printErrs bs errVec
  
  pure $ case res of
    OK vec _ restOfBs -> Right (vec, restOfBs)
    Err (UnexpectedCharacter ch pos) ->
      let [linecol] = posLineCols bs [pos]
       in Left $ ERR ch linecol
    _ -> error "failure should never propagate here"

scanTokens :: ParserT IOMode (MVar ScanErr) ScannerError (Vector Token)
scanTokens = V.fromList <$> many scanToken

simpleScanToken :: ParserT st r e Token
simpleScanToken =
  let returnTok = pure . (`Token` Nothing)
   in $( switch
          [|
            case _ of
              "(" -> returnTok LEFT_PAREN
              ")" -> returnTok RIGHT_PAREN
              "{" -> returnTok LEFT_BRACE
              "}" -> returnTok RIGHT_BRACE
              "," -> returnTok COMMA
              "." -> returnTok DOT
              "-" -> returnTok MINUS
              "+" -> returnTok PLUS
              ";" -> returnTok SEMICOLON
              "*" -> returnTok STAR
              "!" -> returnTok BANG
              "!=" -> returnTok BANG_EQUAL
              "=" -> returnTok EQUAL
              "==" -> returnTok EQUAL_EQUAL
              "<" -> returnTok LESS
              "<=" -> returnTok LESS_EQUAL
              ">" -> returnTok GREATER
              ">=" -> returnTok GREATER_EQUAL
              "/" -> returnTok SLASH
            |]
       )

withError :: ParserT st r e a -> (e -> ParserT st r e a) -> ParserT st r e a
withError (ParserT f) hdl = 
  ParserT $ \foreignPtrContents r eob s int st -> case f foreignPtrContents r eob s int st of
    Err# st' er -> case hdl er of
                    ParserT g -> g foreignPtrContents r eob s int st'
    x -> x

appendScanningErrors :: ScannerError -> ScanErr -> IO ScanErr
appendScanningErrors codeError (ScanErr v)= pure . ScanErr $ V.snoc v codeError

appendMVarScanErrors :: MVar ScanErr -> ScannerError -> IO ()
appendMVarScanErrors mvar codeError 
  = modifyMVar_ mvar (appendScanningErrors codeError)

scanToken :: ParserT IOMode (MVar ScanErr) ScannerError Token
scanToken =  do 
  skipWhiteSpace
  simpleScanToken <|> do
                    pos <- getPos
                    ch <- anyChar
                    mvar <- ask
                    liftIO $ appendMVarScanErrors mvar $ UnexpectedCharacter ch pos
                    scanToken

skipLineComment :: ParserT st r e ()
skipLineComment = branch eof (pure ()) $
  withOption anyWord8
    (\case 10 -> skipWhiteSpace   -- '\n'
           _  -> skipLineComment)
    (pure ())

advance :: ParserT st r e ()
advance = skip 1

skipWhiteSpace :: ParserT st r e ()
skipWhiteSpace = $(switch [| case _ of
              " " -> skipWhiteSpace
              "\r" -> skipWhiteSpace
              "\t" -> skipWhiteSpace
              "\n" -> skipWhiteSpace
              _ -> branch $(string "//") skipLineComment (pure ())
    |])
