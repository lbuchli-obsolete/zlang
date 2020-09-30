module Lib (
  File, Symbol, Decl, Type, Expr, Eval
) where

import Util
import AST
import Parser
import Compiler
import Minimizer
import Prefixer
import TypeChecker
  

build :: String -> Result String Asm
build s = parse s
      >>= Success . prefix
      >>= check
      >>= Success . minimize
      >>= Success . compile
