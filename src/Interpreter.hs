{-# LANGUAGE MonoLocalBinds #-}
{-
The interpreter is used for typechecking (e.g. with dependent types), but should
get the same result as the compiler when run.
-}
module Interpreter where

import           Util
import           IR

type Env = [Eval]

interpret :: File -> Expr -> Result String Expr
interpret = undefined
{-
interpret file expr = reduce initial_env (_eval expr) >>= \eval ->
                      guessType eval >>= \etype ->
                      Success (Expr eval etype)
  where
    initial_env = map (\d -> (_name d, _body d)) file

reduce :: Env -> Eval -> Result String Eval
reduce e (EAp (Expr (ELambda s (Expr a _)) _) (Expr b _))      = reduce ((s, b):e) a
reduce e (EAp (Expr a ta) (Expr b tb))                         = (\a' -> EAp (Expr a' ta) (Expr b tb)) <$> reduce e a
reduce e (ELambda s (Expr x tx))                               = (\x' -> ELambda s (Expr x' tx)) <$> reduce e x
reduce e (EType t)                                             = EType <$> reduceType e t
reduce _ other                                                 = Success other

reduceType :: Env -> Type -> Result String Type
reduceType e (TFn a b)           = TFn <$> reduceType e a <*> reduceType e b
reduceType e (TEither a b)       = TEither <$> reduceType e a <*> reduceType e b
reduceType e (TNamed s t)        = TNamed s <$> reduceType e t
reduceType e (TExpr (Expr x tx)) = (\x' -> TExpr (Expr x' tx)) <$> reduce e x
reduceType _ other               = Success other
-}
