{-
The type checker checks if types are valid and does type inference.
-}
module TypeChecker where

import Data.Bifunctor
import Util
import AST

data TypeError = TypeError {
  _errpos   :: Pos,
  _expected :: Type,
  _actual   :: Type
} deriving Show -- TODO own instance

type Env = [(String, Type)]

check :: File -> Result String File
check file = mapM (first show . checkDecl env) file >>=
  \d's -> if and (map snd d's)
          then check (map fst d's)
          else Success (map fst d's)
  where env = map (\d -> (_name d, _dtype d)) file

-- Returns wheter any types have been infered.
checkDecl :: Env -> Decl -> Result TypeError (Decl, Bool)
checkDecl env d = checkType env (_dtype d) >>= (\(type', changedt) ->
                  checkExpr env (_body d)  >>= (\(body', changedb) ->
                  Success (Decl (_name d) (_pos d) type' body', changedt && changedb)))

checkType :: Env -> Type -> Result TypeError (Type, Bool)
checkType = undefined

checkExpr :: Env -> Expr -> Result TypeError (Expr, Bool)
checkExpr = undefined

                                                 
  
    
