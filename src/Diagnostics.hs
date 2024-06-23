{-# LANGUAGE DataKinds #-}

module Diagnostics (printErrors) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (Foldable (foldl'))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Error.Diagnose
import FlatParse.Stateful (Pos, posLineCols)
import Scanner

diagnoseFile
  :: FilePath -> ByteString -> Vector (ScannerError AsPos) -> Diagnostic Text
diagnoseFile filePath originalFile vecErr =
  foldl'
    ( \diagAccum nextScanErr -> addReport diagAccum (toReport filePath nextScanErr)
    )
    (addFile mempty filePath $ BS.unpack originalFile)
    . refineScannerErrorWithLineCols
    $ vecErr
  where
    refineScannerErrorWithLineCols
      :: Vector (ScannerError AsPos) -> Vector (ScannerError AsLineCol)
    refineScannerErrorWithLineCols vec = V.fromList $ go linecols vec []
      where
        getPositions :: Vector (ScannerError AsPos) -> [Pos]
        getPositions v = concatMap getScannerErrorPos $ V.toList v

        linecols :: [(Int, Int)]
        linecols = fmap fixLineCol $ posLineCols originalFile $ getPositions vecErr
          where
            -- `posLineCols` returns indexes that start from 0,
            -- so we need to fix them for `diagnose`
            fixLineCol :: (Int, Int) -> (Int, Int)
            fixLineCol (x, y) = (x + 1, y + 1)

        go
          :: [(Int, Int)]
          -> Vector (ScannerError AsPos)
          -> [ScannerError AsLineCol]
          -> [ScannerError AsLineCol]
        go lineCols vecErrs resAccum = case V.uncons vecErrs of
          Nothing -> resAccum
          Just (e, errs) -> case e of
            UnexpectedCharacter _ ch -> case lineCols of
              (x : restLineCols) -> go restLineCols errs (UnexpectedCharacter x ch : resAccum)
              _ -> error "Impossible"
            UnterminatedString _ -> case lineCols of
              [] -> error "Impossible"
              (x : restLineCols) -> go restLineCols errs (UnterminatedString x : resAccum)
            InvalidNumberLiteral _ -> case lineCols of
              (x : y : restLineCols) -> go restLineCols errs (InvalidNumberLiteral (x, y) : resAccum)
              _ -> error "Impossible"
            UnexpectedScannerFailure _ -> case lineCols of
              (x : restLineCols) -> go restLineCols errs (UnexpectedScannerFailure x : resAccum)
              _ -> error "Impossible"

    toReport :: FilePath -> ScannerError AsLineCol -> Report Text
    toReport path = \case
      UnexpectedCharacter begin ch ->
        Err
          Nothing
          "Unexpected Character"
          [
            ( Position begin begin path
            , This $ "the character «" <> T.singleton ch <> "» is not valid"
            )
          ]
          []
      UnterminatedString begin ->
        Err
          Nothing
          "Unterminated String"
          [(Position begin begin path, This "the start of the unterminated string")]
          []
      InvalidNumberLiteral (begin, end) ->
        Err
          Nothing
          "Invalid Number Literal"
          [(Position begin end path, This "the invalid number literal")]
          []
      UnexpectedScannerFailure begin ->
        Err
          Nothing
          "Unexpected Scanner Failure"
          [(Position begin begin path, This "here it failed")]
          [Note "THIS SHOULD NEVER HAPPEN, please report it."]

printErrors :: FilePath -> ByteString -> Vector (ScannerError AsPos) -> IO ()
printErrors filePath originalFile vecErr =
  printDiagnostic
    stderr
    WithUnicode
    (TabSize 2)
    defaultStyle
    diagnostic
  where
    diagnostic = diagnoseFile filePath originalFile vecErr
