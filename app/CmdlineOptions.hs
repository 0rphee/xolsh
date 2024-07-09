{-# LANGUAGE TemplateHaskell #-}

module CmdlineOptions
  ( options
  , Options (..)
  , execParser
  )
where

import Data.Version (showVersion)
import GitHash
import Options.Applicative
import Paths_xolsh (version)

newtype Options = Options
  { sourceCodeFile :: Maybe FilePath
  }

options :: ParserInfo Options
options =
  info
    (opts <**> helper)
    ( fullDesc
        <> header headerString
        <> progDesc "a lox interpreter written in haskell"
        <> footer
          "still a work in progress, source code here: https://codeberg.org/0rphee/xolsh, \
          \following the Crafting Interpreters book: https://craftinginterpreters.com/"
        <> failureCode 64
    )
  where
    headerString = concat ["xolsh - v", showVersion version, gitInfoString]
    gitInfoString = case $$tGitInfoCwdTry of
      Right gi ->
        concat
          [ " - "
          , giBranch gi
          , "@"
          , giHash gi
          , " ("
          , show $ giCommitCount gi
          , " commits in HEAD"
          , ")"
          ]
      _ -> ""

opts :: Parser Options
opts =
  Options
    <$> optional
      ( strArgument
          ( metavar "FILENAME"
              <> help "Input file"
              <> action "directory"
              <> action "file"
          )
      )
      <**> simpleVersioner (showVersion version)
