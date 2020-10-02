{-# LANGUAGE MonoLocalBinds #-}
{-
The interpreter is used for typechecking (e.g. with dependent types), but should
get the same result as the compiler when run.
-}
module Interpreter where

import Util
import AST

type IEnv = [(String, Eval)]

interpret :: File -> Expr -> Result String Expr
interpret file expr = reduce initial_env (_eval expr) >>= \eval ->
                      guessType eval >>= \etype ->
                      Success (Expr eval etype)
  where
    initial_env = map (\d -> (_name d, _body d)) file

reduce :: IEnv -> Eval -> Result String Eval
reduce e (EAp (Expr (ELambda s (Expr a _)) _) (Expr b _))      = reduce ((s, b):e) a
reduce e (EAp (Expr a ta) (Expr b tb))                         = (\a' -> EAp (Expr a' ta) (Expr b tb)) <$> reduce e a
reduce e (ELambda s (Expr x tx))                               = (\x' -> ELambda s (Expr x' tx)) <$> reduce e x
reduce e (EType t)                                             = EType <$> reduceType e t
reduce e (EVar s)                                              = case lookup s e of
                                                                   Just eval -> Success eval
                                                                   Nothing   -> Success (EVar s)
reduce _ other                                                 = Success other

reduceType :: IEnv -> Type -> Result String Type
reduceType e (TFn a b)           = TFn <$> reduceType e a <*> reduceType e b
reduceType e (TNamed s t)        = TNamed s <$> reduceType e t
reduceType e (TExpr (Expr x tx)) = (\x' -> TExpr (Expr x' tx)) <$> reduce e x
reduceType _ other               = Success other
