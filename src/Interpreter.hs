{-
The interpreter is used for typechecking (e.g. with dependent types), but should
get the same result as the compiler when run.
-}
module Interpreter where

import Util
import AST

type RuntimeError = String

interpret :: File -> Result RuntimeError Expr
interpret = undefined
