{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Scanner (scanTokens) where

import Bluefin.Eff
import Bluefin.IO (IOE)
import Bluefin.State (State)
import Bluefin.State qualified as State
import Bluefin.Writer (Writer)
import Control.Monad (when)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Char (isAlpha, isDigit)
import Data.Functor ((<&>))
import Data.Vector (Vector)
import Error qualified
import TokenType qualified
import VectorBuilder.Builder as VB
import VectorBuilder.Vector as VB

data Scanner = Scanner
  { source :: !ByteString,
    tokens :: !(VB.Builder TokenType.Token),
    start :: !Int,
    current :: !Int,
    line :: !Int
  }

substring :: ByteString -> Int -> Int -> ByteString
substring bs start end = BS.take (end - start) (BS.drop start bs)

myIsAlpha :: Char -> Bool
myIsAlpha c = isAlpha c || c == '_'

myIsAlphaNum :: Char -> Bool
myIsAlphaNum c = myIsAlpha c || isDigit c

scanTokens ::
  (io :> es, w :> es) =>
  IOE io ->
  Writer Error.ErrorPresent w ->
  ByteString ->
  Eff es (Vector TokenType.Token)
scanTokens io w source =
  State.evalState initialScanner $ \st ->
    scanTokensHelper io st w
  where
    initialScanner =
      Scanner
        { source = source,
          tokens = VB.empty,
          start = 0,
          current = 0,
          line = 1
        }

scanTokensHelper ::
  forall es io st w.
  (io :> es, st :> es, w :> es) =>
  IOE io ->
  State Scanner st ->
  Writer Error.ErrorPresent w ->
  Eff es (Vector TokenType.Token)
scanTokensHelper io st w = do
  whileM
    (not <$> isAtEnd)
    ( do
        State.modify st $ \scanner -> scanner {start = scanner.current}
        scanToken
    )
  State.modify st $ \sc ->
    sc
      { tokens =
          sc.tokens
            <> VB.singleton
              ( TokenType.Token
                  { TokenType.ttype = TokenType.EOF,
                    TokenType.lexeme = "",
                    TokenType.tline = sc.line
                  }
              )
      }

  State.get st <&> (VB.build . (.tokens))
  where
    isAtEnd :: Eff es Bool
    isAtEnd = do
      scanner <- State.get st
      pure $ scanner.current >= BS.length scanner.source
    scanToken :: Eff es ()
    scanToken = do
      c <- advance
      case c of
        '(' -> addToken1 TokenType.LEFT_PAREN
        ')' -> addToken1 TokenType.RIGHT_PAREN
        '{' -> addToken1 TokenType.LEFT_BRACE
        '}' -> addToken1 TokenType.RIGHT_BRACE
        ',' -> addToken1 TokenType.COMMA
        '.' -> addToken1 TokenType.DOT
        '-' -> addToken1 TokenType.MINUS
        '+' -> addToken1 TokenType.PLUS
        ';' -> addToken1 TokenType.SEMICOLON
        '*' -> addToken1 TokenType.STAR
        '!' ->
          match '='
            >>= addToken1 . (\cond -> if cond then TokenType.BANG_EQUAL else TokenType.BANG)
        '=' ->
          match '='
            >>= addToken1 . (\cond -> if cond then TokenType.EQUAL_EQUAL else TokenType.EQUAL)
        '<' ->
          match '='
            >>= addToken1 . (\cond -> if cond then TokenType.LESS_EQUAL else TokenType.LESS)
        '>' ->
          match '='
            >>= addToken1
              . (\cond -> if cond then TokenType.GREATER_EQUAL else TokenType.GREATER)
        '/' -> do
          match '/' >>= \case
            True ->
              whileM
                ( do
                    pc <- peek
                    end <- isAtEnd
                    pure $ pc /= '\n' && not end
                )
                advance
            False -> addToken1 TokenType.SLASH
        -- ignore whitespace
        ' ' -> pure ()
        '\r' -> pure ()
        '\t' -> pure ()
        '\n' -> State.modify st $ \sc -> sc {line = sc.line + 1}
        '"' -> string
        _ ->
          if
            | isDigit c -> number
            | myIsAlpha c -> identifier
            | otherwise -> do
                sc <- State.get st
                Error.scanError io w sc.line "Unexpected character."
    advance :: Eff es Char
    advance = do
      oldScanner <- State.get st
      let newScanner = oldScanner {current = oldScanner.current + 1}
      State.put st newScanner
      pure $ BS.index oldScanner.source oldScanner.current
    addToken1 :: TokenType.TokenType -> Eff es ()
    addToken1 = addToken2
    addToken2 :: TokenType.TokenType -> Eff es ()
    addToken2 ttype = State.modify st $ \sc ->
      -- substring
      let text = SBS.toShort $ substring sc.source sc.start sc.current
       in sc {tokens = sc.tokens <> VB.singleton (TokenType.Token ttype text sc.line)}
    match :: Char -> Eff es Bool
    match expected = do
      e <- isAtEnd
      sc <- State.get st
      if
        | e -> pure False
        | BS.index sc.source sc.current /= expected -> pure False
        | otherwise -> do
            State.put st $ sc {current = sc.current + 1}
            pure True
    peek :: Eff es Char
    peek = do
      isAtEnd >>= \case
        True -> pure '\0'
        False -> do
          sc <- State.get st
          pure $ BS.index sc.source sc.current
    string :: Eff es ()
    string = do
      whileM
        ( do
            c <- peek
            e <- isAtEnd
            pure $ c /= '"' && not e
        )
        ( do
            c <- peek
            when ('\n' == c) $ State.modify st $ \sc -> sc {line = sc.line + 1}
            advance
        )
      e <- isAtEnd
      if e
        then do
          l <- (.line) <$> State.get st
          Error.scanError io w l "Unterminated string."
          pure ()
        else do
          advance
          sc <- State.get st
          let value = SBS.toShort $ substring sc.source (sc.start + 1) (sc.current - 1)
          addToken2 (TokenType.STRING $ value)
    number :: Eff es ()
    number = do
      whileM
        (isDigit <$> peek)
        advance
      -- look for fractional part
      p1 <- peek
      p2 <- peekNext
      when
        (p1 == '.' && isDigit p2)
        (advance {- consume the "." -} >> whileM (isDigit <$> peek) advance)
      sc <- State.get st
      addToken2
        (TokenType.NUMBER $ read . BS.unpack $ substring sc.source sc.start sc.current)
    peekNext :: Eff es Char
    peekNext = do
      State.get st <&> \sc ->
        if sc.current + 1 >= BS.length sc.source
          then '\0'
          else BS.index sc.source (sc.current + 1)
    identifier :: Eff es ()
    identifier = do
      whileM
        (myIsAlphaNum <$> peek)
        advance
      sc <- State.get st
      let tokType = identOrKeywTokTy $ substring sc.source sc.start sc.current
      addToken1 tokType

{-# INLINE whileM #-}
{-# SPECIALIZE whileM :: Eff es Bool -> Eff es Bool -> Eff es () #-}
whileM :: (Monad m) => m Bool -> m a -> m ()
whileM cond act = do
  r <- cond
  when r $ act >> whileM cond act

identOrKeywTokTy :: ByteString -> TokenType.TokenType
identOrKeywTokTy = \case
  "and" -> TokenType.AND
  "class" -> TokenType.CLASS
  "else" -> TokenType.ELSE
  "false" -> TokenType.FALSE
  "for" -> TokenType.FOR
  "fun" -> TokenType.FUNN
  "if" -> TokenType.IF
  "nil" -> TokenType.NIL
  "or" -> TokenType.OR
  "print" -> TokenType.PRINT
  "return" -> TokenType.RETURN
  "super" -> TokenType.SUPER
  "this" -> TokenType.THIS
  "true" -> TokenType.TRUE
  "var" -> TokenType.VAR
  "while" -> TokenType.WHILE
  _ -> TokenType.IDENTIFIER
