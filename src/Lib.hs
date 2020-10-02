module Lib (
  Asm, build, typeCheck
) where

import AST
import Util
import Parser
import Compiler
import Minimizer
import Prefixer
import TypeChecker
  

typeCheck :: String -> Result String File
typeCheck s = parse s
          >>= Success . prefix
          >>= check

build :: String -> Result String Asm
build s = parse s
      >>= Success . prefix
      >>= check
      >>= Success . minimize
      >>= Success . compile
