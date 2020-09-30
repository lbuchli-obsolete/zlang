{-# LANGUAGE MonoLocalBinds #-}
{-
The interpreter is used for typechecking (e.g. with dependent types), but should
get the same result as the compiler when run.
-}
module Interpreter where

import Util
import AST

interpret :: File -> Expr -> Result String Expr
interpret = undefined
