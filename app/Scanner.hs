{-# LANGUAGE TemplateHaskell #-}

module Scanner where

import Data.ByteString.Char8 as B
import Data.Vector as V
import FlatParse.Basic
import Token

data ScannerState = ScannerState
  { source :: !ByteString
  , tokens :: !(Vector Token)
  }

a =
  $( switch
      [|
        case _ of
          "(" -> pure LEFT_PAREN
        |]
   )
