{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Scanner where

import Control.Monad (when)
import Control.Monad.RWS.CPS
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAlpha, isDigit)
import Data.Functor ((<&>))
import TokenType (Literal (..), Token (..), TokenType (..))

data Scanner = Scanner
  { source :: !ByteString
  , tokens :: ![Token]
  , start :: !Int
  , current :: !Int
  , line :: !Int
  }
  deriving (Show)

type Error = String

type ScanM r a = RWS r [Error] Scanner a

substring :: ByteString -> Int -> Int -> ByteString
substring bs start end = BS.take (end - start) (BS.drop start bs)

myIsAlpha :: Char -> Bool
myIsAlpha c = isAlpha c || c == '_'

myIsAlphaNum :: Char -> Bool
myIsAlphaNum c = myIsAlpha c || isDigit c

scanTokens :: ByteString -> ([Token], [Error])
scanTokens source = (\act -> evalRWS act () initialScanner) $ do
  whileM
    (not <$> isAtEnd)
    ( do
        modify' $ \scanner -> scanner {start = scanner.current}
        scanToken
    )
  modify' $ \sc ->
    sc
      { tokens =
          Token {ttype = EOF, lexeme = "", literal = NoLit, tline = sc.line}
            : sc.tokens
      }

  get <&> (.tokens)
  where
    initialScanner =
      Scanner
        { source = source
        , tokens = []
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
        '!' ->
          match '=' >>= addToken1 . (\cond -> if cond then BANG_EQUAL else BANG)
        '=' ->
          match '=' >>= addToken1 . (\cond -> if cond then EQUAL_EQUAL else EQUAL)
        '<' ->
          match '=' >>= addToken1 . (\cond -> if cond then LESS_EQUAL else LESS)
        '>' ->
          match '=' >>= addToken1 . (\cond -> if cond then GREATER_EQUAL else GREATER)
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
            False -> addToken1 SLASH
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
                lerror sc.line "Unexpected character"
    advance :: forall r. ScanM r Char
    advance = do
      oldScanner <- get
      let newScanner = oldScanner {current = oldScanner.current + 1}
      put newScanner
      pure $ BS.index oldScanner.source oldScanner.current
    addToken1 :: forall r. TokenType -> ScanM r ()
    addToken1 ttype = addToken2 ttype NoLit
    addToken2 :: forall r. TokenType -> Literal -> ScanM r ()
    addToken2 ttype literal = modify' $ \sc ->
      -- substring
      let text = substring sc.source sc.start sc.current
       in sc {tokens = Token ttype text literal sc.line : sc.tokens}
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
            when ('\n' /= c) $ modify' $ \sc -> sc {line = sc.line + 1}
            advance
        )
      e <- isAtEnd
      if e
        then do
          l <- (.line) <$> get
          lerror l "Unterminated string."
          pure ()
        else do
          advance
          sc <- get
          let value = substring sc.source (sc.start + 1) (sc.current - 1)
          addToken2 STRING (LitStr value)
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
        NUMBER
        (LitNum $ read . BS.unpack $ substring sc.source sc.start sc.current)
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

lerror :: forall r. Int -> String -> ScanM r ()
lerror line = report line ""

report :: forall r. Int -> String -> String -> ScanM r ()
report line location msg = do
  let er = mconcat ["[line ", show line, "] Error", location, ": ", msg]
  tell [er]

identOrKeywTokTy :: ByteString -> TokenType
identOrKeywTokTy = \case
  "and" -> AND
  "class" -> CLASS
  "else" -> ELSE
  "false" -> FALSE
  "for" -> FOR
  "fun" -> FUN
  "if" -> IF
  "nil" -> NIL
  "or" -> OR
  "print" -> PRINT
  "return" -> RETURN
  "super" -> SUPER
  "this" -> THIS
  "true" -> TRUE
  "var" -> VAR
  "while" -> WHILE
  _ -> IDENTIFIER
