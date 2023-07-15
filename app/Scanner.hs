{-# LANGUAGE TemplateHaskell #-}

module Scanner where

import Data.ByteString.Char8 as B
import Data.Vector as V
import FlatParse.Basic
import Token

data ScannerError = UnexpectedCharacter Char Pos deriving (Show)

data CodeError = ERR Char (Int, Int) deriving (Show)

data ScannerState = ScannerState
  { source :: !ByteString
  , tokens :: !(Vector Token)
  }

scanFile :: ByteString -> Either CodeError (Vector Token, ByteString)
scanFile bs = case runParser scanTokens bs of
  OK vec restOfBs -> Right (vec, restOfBs)
  Err (UnexpectedCharacter ch pos) ->
    let [linecol] = posLineCols bs [pos]
     in Left $ ERR ch linecol
  _ -> error "failure should never propagate here"

scanTokens :: ParserT st ScannerError (V.Vector Token)
scanTokens = V.fromList <$> many scanToken

scanToken :: ParserT st ScannerError Token
scanToken =
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
              "//" -> skipRestOfComment >> scanToken
              " " -> scanToken
              "\r" -> scanToken
              "\t" -> scanToken
              "\n" -> scanToken
              _ -> do
                pos <- getPos
                ch <- anyChar
                err $ UnexpectedCharacter ch pos
            |]
       )

skipRestOfComment :: ParserT st e ()
skipRestOfComment = do
  skipMany $ skipSatisfy (/= '\n')
