{-
The type checker checks if types are valid and does type inference.
-}
module TypeChecker where

import Util
import AST

data TypeError = TypeError {
  _pos      :: Pos,
  _expected :: Type,
  _actual   :: Type
}

check :: File -> Result TypeError File
check = undefined
