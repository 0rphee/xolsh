{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Scanner (scanTokens, whileM) where

import Control.Monad (when)
import Control.Monad.RWS.CPS
  ( RWST
  , evalRWST
  )
import Control.Monad.State.Class (MonadState (get, put), modify')
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAlpha, isDigit)
import Data.Functor ((<&>))
import Data.Vector (Vector)
import Error qualified
import TokenType qualified
import VectorBuilder.Builder as VB
import VectorBuilder.Vector as VB

data Scanner = Scanner
  { source :: !ByteString
  , tokens :: !(VB.Builder TokenType.Token)
  , start :: !Int
  , current :: !Int
  , line :: !Int
  }

type ScanM r a = RWST r Error.ErrorPresent Scanner IO a

substring :: ByteString -> Int -> Int -> ByteString
substring bs start end = BS.take (end - start) (BS.drop start bs)

myIsAlpha :: Char -> Bool
myIsAlpha c = isAlpha c || c == '_'

myIsAlphaNum :: Char -> Bool
myIsAlphaNum c = myIsAlpha c || isDigit c

scanTokens :: ByteString -> IO (Vector TokenType.Token, Error.ErrorPresent)
scanTokens source = (\act -> evalRWST act () initialScanner) $ do
  whileM
    (not <$> isAtEnd)
    ( do
        modify' $ \scanner -> scanner {start = scanner.current}
        scanToken
    )
  modify' $ \sc ->
    sc
      { tokens =
          sc.tokens
            <> VB.singleton
              ( TokenType.Token
                  { TokenType.ttype = TokenType.EOF
                  , TokenType.lexeme = ""
                  , TokenType.tline = sc.line
                  }
              )
      }

  get <&> (VB.build . (.tokens))
  where
    initialScanner =
      Scanner
        { source = source
        , tokens = VB.empty
        , start = 0
        , current = 0
        , line = 1
        }
    isAtEnd :: forall r. ScanM r Bool
    isAtEnd = do
      scanner <- get
      pure $ scanner.current >= BS.length scanner.source
    scanToken :: forall r. ScanM r ()
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
        '\n' -> modify' $ \sc -> sc {line = sc.line + 1}
        '"' -> string
        _ ->
          if
            | isDigit c -> number
            | myIsAlpha c -> identifier
            | otherwise -> do
                sc <- get
                Error.scanError sc.line "Unexpected character."
    advance :: forall r. ScanM r Char
    advance = do
      oldScanner <- get
      let newScanner = oldScanner {current = oldScanner.current + 1}
      put newScanner
      pure $ BS.index oldScanner.source oldScanner.current
    addToken1 :: forall r. TokenType.TokenType -> ScanM r ()
    addToken1 = addToken2
    addToken2 :: forall r. TokenType.TokenType -> ScanM r ()
    addToken2 ttype = modify' $ \sc ->
      -- substring
      let text = substring sc.source sc.start sc.current
       in sc {tokens = sc.tokens <> VB.singleton (TokenType.Token ttype text sc.line)}
    match :: forall r. Char -> ScanM r Bool
    match expected = do
      e <- isAtEnd
      sc <- get
      if
        | e -> pure False
        | BS.index sc.source sc.current /= expected -> pure False
        | otherwise -> do
            put $ sc {current = sc.current + 1}
            pure True
    peek :: forall r. ScanM r Char
    peek = do
      isAtEnd >>= \case
        True -> pure '\0'
        False -> do
          sc <- get
          pure $ BS.index sc.source sc.current
    string :: forall r. ScanM r ()
    string = do
      whileM
        ( do
            c <- peek
            e <- isAtEnd
            pure $ c /= '"' && not e
        )
        ( do
            c <- peek
            when ('\n' == c) $ modify' $ \sc -> sc {line = sc.line + 1}
            advance
        )
      e <- isAtEnd
      if e
        then do
          l <- (.line) <$> get
          Error.scanError l "Unterminated string."
          pure ()
        else do
          advance
          sc <- get
          let value = substring sc.source (sc.start + 1) (sc.current - 1)
          addToken2 (TokenType.STRING value)
    number :: forall r. ScanM r ()
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
      sc <- get
      addToken2
        (TokenType.NUMBER $ read . BS.unpack $ substring sc.source sc.start sc.current)
    peekNext :: forall r. ScanM r Char
    peekNext = do
      get <&> \sc ->
        if sc.current + 1 >= BS.length sc.source
          then '\0'
          else BS.index sc.source (sc.current + 1)
    identifier :: forall r. ScanM r ()
    identifier = do
      whileM
        (myIsAlphaNum <$> peek)
        advance
      sc <- get
      let tokType = identOrKeywTokTy $ substring sc.source sc.start sc.current
      addToken1 tokType

{-# INLINE whileM #-}
{-# SPECIALIZE whileM :: ScanM r Bool -> ScanM r Bool -> ScanM r () #-}
whileM :: Monad m => m Bool -> m a -> m ()
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
