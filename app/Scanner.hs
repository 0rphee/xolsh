{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module Scanner (scanFile, ScannerError(..), CodeError(..), ScannerState(..), Scanner (..)) where

import Control.Monad.Writer.Strict
import Data.ByteString.Char8 qualified as B
import Data.Vector qualified as V
import FlatParse.Basic
import Token
import Data.ByteString.Char8 (ByteString)
import Data.Vector (Vector)

data ScannerError = UnexpectedCharacter Char Pos deriving (Show)

data CodeError = ERR Char (Int, Int) deriving (Show)

data ScannerState = ScannerState
  { source :: !ByteString
  , tokens :: !(Vector Token)
  }

newtype Scanner err parsingRes = Scanner {getScanner :: WriterT (Vector err) (Parser err) parsingRes}
  deriving (Functor, Applicative, Monad, MonadWriter (Vector err))

scanFile :: ByteString -> Either CodeError (Vector Token, ByteString)
scanFile bs = case runParser scanTokens bs of
  OK vec restOfBs -> Right (vec, restOfBs)
  Err (UnexpectedCharacter ch pos) ->
    let [linecol] = posLineCols bs [pos]
     in Left $ ERR ch linecol
  _ -> error "failure should never propagate here"

scanTokens :: ParserT st ScannerError (V.Vector Token)
scanTokens = V.fromList <$> many scanToken

toScanner :: Parser ScannerError r -> Scanner ScannerError r
toScanner parser = Scanner $ lift parser

simpleScanToken :: ParserT st ScannerError Token
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

withError :: ParserT st e a -> (e -> ParserT st e a) -> ParserT st e a
withError (ParserT f) hdl = 
  ParserT $ \foreignPtrContents eob s st -> case f foreignPtrContents eob s st of
    Err# st' er -> case hdl er of
                    ParserT g -> g foreignPtrContents eob s st'
    x -> x

  
-- scanToken' = do 
--   toScanner $ withOption (skipWhiteSpace >> simpleScanToken) 
--                 \ tok ->

--                           <|> do
--                             pos <- getPos
--                             ch <- anyChar

--                             err $ UnexpectedCharacter ch pos


scanToken :: ParserT st ScannerError Token
scanToken =  skipWhiteSpace >> simpleScanToken
                          <|> do
                            pos <- getPos
                            ch <- anyChar
                            err $ UnexpectedCharacter ch pos

skipLineComment :: ParserT st e ()
skipLineComment = branch eof (pure ()) $
  withOption anyWord8
    (\case 10 -> skipWhiteSpace   -- '\n'
           _  -> skipLineComment)
    (pure ())

advance :: ParserT st e ()
advance = skip 1

skipWhiteSpace :: ParserT st e ()
skipWhiteSpace = $(switch [| case _ of
              " " -> skipWhiteSpace
              "\r" -> skipWhiteSpace
              "\t" -> skipWhiteSpace
              "\n" -> skipWhiteSpace
              _ -> branch $(string "//") skipLineComment (pure ())
    |])
