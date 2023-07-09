module CmdlineOptions
  ( options
  , Options (..)
  , execParser
  )
where

import Options.Applicative

newtype Options = Options
  { sourceCodefile :: Maybe FilePath
  }

options :: ParserInfo Options
options =
  info
    (opts <**> helper)
    ( fullDesc
        <> header "xolsh - a lox interpreter written in haskell "
        <> footer
          "still a work in progress, source code here: https://codeberg.org/0rphee/xolsh, \
          \following the Crafting Interpreters book: https://craftinginterpreters.com/"
    )

opts :: Parser Options
opts = Options <$> inputFilePath

inputFilePath :: Parser (Maybe FilePath)
inputFilePath =
  optional $
    strArgument
      ( metavar "FILENAME"
          <> help "Input file"
          <> action "directory"
          <> action "file"
      )
