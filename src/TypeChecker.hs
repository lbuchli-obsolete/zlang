{-# LANGUAGE LambdaCase #-}
{-
The type checker checks if types are valid and does type inference.
-}
module TypeChecker where

import           Control.Monad
import           Data.Foldable
import           Data.Bifunctor
import           Interpreter
import           Util
import           IR

type LocalEnv = [Type]

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
passEval f env (EAp a@(_, ta) b@(_, tb)) =
  combine f ta (TFn tb TAny) >>= \(c, ta') -> passExpr f env a >>= \(c', a') ->
    passExpr f env b >>= \(c'', b') ->
      combine f ta' (snd a') >>= \(c''', ta'') ->
        combine f tb (snd b') >>= \(c'''', tb') ->
          apResultType f env ta'' tb' >>= \t -> Success
            ( c || c' || c'' || c''' || c''''
            , (EAp (fst a', ta'') (fst b', tb'), t) -- TODO detect type
            )
passEval f env (ELambda s x) =
  second (\x' -> (ELambda s x, TFn TAny (snd x'))) <$> passExpr f (TAny : env) x
passEval f env (EType t) =
  (\(c, _, t') -> (c, (EType t', TType))) <$> passType f env t
passEval f env (EList xs) = -- TODO combine all elements' types
  second (\x -> (EList x, TList TAny))
    <$> foldM (\(c, xs') x -> bimap (|| c) (: xs') <$> passExpr f env x)
              (False, [])
              xs
passEval _ env (  ELocalRef i) = Success (False, (ELocalRef i, env !! i))
passEval f env c@(ECall i n  ) = (\t -> (False, (c, t))) <$> callType f env i n
passEval _ _   other           = Success (False, (other, TAny))

callType :: File -> LocalEnv -> Int -> Int -> Result String Type
callType f _ i _ = Success $ snd (f !! i)

apResultType :: File -> LocalEnv -> Type -> Type -> Result String Type
apResultType _ _ _ _ = Success TAny -- TODO

passType :: File -> LocalEnv -> Type -> Result String (Bool, LocalEnv, Type)
passType f env (TFn a b) = passType f env a >>= \(c, env', a') ->
  passType f env' b >>= \(c', env'', b') -> Success (c || c', env'', TFn a' b')
passType f env (TEither a b) = passType f env a >>= \(c, env', a') ->
  passType f env' b
    >>= \(c', env'', b') -> Success (c || c', env'', TEither a' b')
passType f env (TNamed s t) =
  (\(c, env', t') -> (c, env', TNamed s t')) <$> passType f env t -- TODO recursive types?
passType f env (TExpr e@(_, t)) =
  combine f t TType >>= \(c, t') -> interpret f e >>= \e' ->
    combine f t' (snd e') >>= \(c', t'') ->
      Success (c || c' || e /= e', env, (TExpr (fst e', t'')))
passType f env (TList t) =
  (\(c, env', t') -> (c, env', TList t')) <$> passType f env t
passType _ env other = Success (False, env, other)

combine :: File -> Type -> Type -> Result String (Bool, Type)
combine f (TNamed _ a) b            = combine f a b
combine f a            (TNamed _ b) = combine f a b
combine _ TAny         b            = Success (True, b)
combine _ a            TAny         = Success (True, a)
combine f (TFn a b)    (TFn a' b')  = combine f a a' >>= \(ca, a'') ->
  combine f b b' >>= \(cb, b'') -> Success (ca || cb, TFn a'' b'')
combine _ TType     TType     = Success (False, TType)
combine f (TList a) (TList b) = second TList <$> combine f a b
combine f (TExpr e) b         = interpret f e >>= toType >>= \a -> combine f a b
combine f a         (TExpr e) = interpret f e >>= toType >>= \b -> combine f a b
combine _ a b | a == b        = Success (False, a)
combine _ a b =
  Error
    $  "Cannot combine type "
    ++ show a
    ++ " with type "
    ++ show b
    ++ " (type checking phase)"


toType :: Expr -> Result String Type
toType ((EType t), _) = Success t
toType _              = Error "Expression is not of type type"
