{-
The minimizer translates type-annotated code to a simpler
representation that the compiler can easily translate.
-}
module Minimizer where

import AST
import Data.Word

data MFile = MFile -- TODO

data MExpr = MAp MExpr MExpr
           | MLambda Symbol MExpr
           | MTFn MExpr MExpr
           | MTNamed Symbol MExpr
           | MTType
           | MTAny
           | MI64 Int
           | MF64 Float
           | MStr String
           | MChar Char
           | MByte Word8
           | MPtr Int
           | MVar Symbol

minimize :: File -> MFile
minimize = undefined

minimizeExpr :: Expr -> MExpr
minimizeExpr = undefined
