module Diagnostics (printErrors) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (Foldable (foldl'))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace
import Error.Diagnose
import Error.Diagnose.Report (pattern Err, pattern Warn)
import FlatParse.Stateful (Pos, posLineCols)
import Scanner

diagnoseFile :: FilePath -> ByteString -> Vector ScannerError -> Diagnostic Text
diagnoseFile filePath originalFile vecErr =
  foldl'
    ( \diagAccum nextScanErr -> addReport diagAccum (toReport filePath nextScanErr)
    )
    (addFile mempty filePath $ BS.unpack originalFile)
    . refineScannerErrorWithLineCols
    $ vecErr
  where
    refineScannerErrorWithLineCols :: Vector ScannerError -> Vector ScannerError
    refineScannerErrorWithLineCols vec = V.fromList $ go linecols vec []
      where
        getPositions :: Vector ScannerError -> [Pos]
        getPositions v = concatMap getScannerErrorPos $ V.toList v

        linecols :: [(Int, Int)]
        linecols = fmap fixLineCol $ posLineCols originalFile $ getPositions vecErr
          where
            -- `posLineCols` returns indexes that start from 0,
            -- so we need to fix them for `diagnose`
            fixLineCol :: (Int, Int) -> (Int, Int)
            fixLineCol (x, y) = (x + 1, y + 1)

        go :: [(Int, Int)] -> Vector ScannerError -> [ScannerError] -> [ScannerError]
        go lineCols vecErrs resAccum = case V.uncons vecErrs of
          Nothing -> resAccum
          Just (e, errs) -> case e of
            UnexpectedCharacter p _ ch -> case lineCols of
              (x : restLineCols) -> go restLineCols errs (UnexpectedCharacter p (Just x) ch : resAccum)
              _ -> error "Impossible"
            UnterminatedString p _ -> case lineCols of
              [] -> error "Impossible"
              (x : restLineCols) -> go restLineCols errs (UnterminatedString p (Just x) : resAccum)
            InvalidNumberLiteral ps _ bs -> case lineCols of
              (x : y : restLineCols) -> go restLineCols errs (InvalidNumberLiteral ps (Just (x, y)) bs : resAccum)
              _ -> error "Impossible"
            UnexpectedScannerFailure p _ -> case lineCols of
              (x : restLineCols) -> go restLineCols errs (UnexpectedScannerFailure p (Just x) : resAccum)
              _ -> error "Impossible"

    toReport :: FilePath -> ScannerError -> Report Text
    toReport path = \case
      UnexpectedCharacter _ (Just begin) ch ->
        Err
          Nothing
          "Unexpected Character"
          [
            ( Position begin begin path
            , This $ "the character «" <> T.singleton ch <> "» is not valid"
            )
          ]
          []
      UnterminatedString _ (Just begin) ->
        Err
          Nothing
          "Unterminated String"
          [(Position begin begin path, This "the start of the unterminated string")]
          []
      InvalidNumberLiteral _ (Just (begin, end)) _ ->
        Err
          Nothing
          "Invalid Number Literal"
          [(Position begin end path, This "the invalid number literal")]
          []
      UnexpectedScannerFailure _ (Just begin) ->
        Err
          Nothing
          "Unexpected Scanner Failure"
          [(Position begin begin path, This "here it failed")]
          [Note "THIS SHOULD NEVER HAPPEN, please report it."]
      _ -> error "Impossible"

-- it is guaranteed that by this point all scanner errors have their positions in terms of line/columns

printErrors :: FilePath -> ByteString -> Vector ScannerError -> IO ()
printErrors filePath originalFile vecErr =
  printDiagnostic
    stderr
    WithUnicode
    (TabSize 2)
    defaultStyle
    diagnostic
  where
    diagnostic = diagnoseFile filePath originalFile vecErr
