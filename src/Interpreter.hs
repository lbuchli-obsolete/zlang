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
interpret file expr = reduce file [] (fst expr)
  >>= \eval -> Success (guessType eval) >>= \etype -> Success (eval, etype)

reduce :: File -> Env -> Eval -> Result String Eval
reduce f e (EAp (ELambda _ (a, _), _) (b, _)) = reduce f (b : e) a
reduce f e (EAp (a, ta) (b, tb)) =
  (\a' -> EAp (a', ta) (b, tb)) <$> reduce f e a
reduce f e (ELambda s (x, tx)) = (\x' -> ELambda s (x', tx)) <$> reduce f e x
reduce f e (EType     t      ) = EType <$> reduceType f e t
reduce _ e (ELocalRef i      ) = Success (e !! i)
reduce f e (ECall i n        ) = undefined
reduce _ _ other               = Success other

reduceType :: File -> Env -> Type -> Result String Type
reduceType f e (TFn a b) = TFn <$> reduceType f e a <*> reduceType f e b
reduceType f e (TEither a b) =
  TEither <$> reduceType f e a <*> reduceType f e b
reduceType f e (TNamed s t   ) = TNamed s <$> reduceType f e t
reduceType f e (TExpr (x, tx)) = (\x' -> TExpr (x', tx)) <$> reduce f e x
reduceType _ _ other           = Success other
