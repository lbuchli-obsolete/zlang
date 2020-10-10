{-# LANGUAGE LambdaCase #-}
{-
The type checker checks if types are valid and does type inference.
-}
module TypeChecker where

import           Data.Foldable
import           Data.Bifunctor
import           Util
import           IR

type LocalEnv = [(Symbol, Type)]

check :: File -> Result String File
check f =
  passCheck f >>= \(changed, f') -> if changed then check f' else Success f'

passCheck :: File -> Result String (Bool, File)
passCheck file = foldrM -- TODO fold or foldr?
  (\expr (changed, f) -> bimap (changed ||) (: f) <$> passExpr file [] expr)
  (False, [])
  file

passExpr :: File -> LocalEnv -> Expr -> Result String (Bool, Expr)
passExpr f env (e, t) = passType f env t >>= \(c, env', t') ->
  passEval f env' e >>= \(c', (e', t'')) -> bimap show id (combine f t' t'')
    >>= \(_, t''') -> Success (c || c' || (t /= t'''), (e', t'''))

passEval :: File -> LocalEnv -> Eval -> Result String (Bool, Expr)
passEval f env (EAp     a b) = undefined
passEval f env (ELambda s x) = undefined
passEval f env (EType     t) = undefined
passEval f env (ELocalRef i) = undefined
passEval f env (ECall i n  ) = undefined
passEval _ _   _             = undefined

passType :: File -> LocalEnv -> Type -> Result String (Bool, LocalEnv, Type)
passType f env (TFn     a b) = undefined
passType f env (TEither a b) = undefined
passType f env (TNamed  s t) = undefined
passType f env TType         = Success (False, env, TType)
passType f env TAny          = Success (False, env, TAny)
passType f env (TExpr  x)    = undefined
passType f env (TToken s)    = undefined
