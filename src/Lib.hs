module Lib
  ( typeCheck
  , Type
  )
where

import           Util
import           Parser
import           TypeChecker
import           Translator
import           IR


typeCheck :: String -> Result String File
typeCheck s = parse s >>= translate >>= check
