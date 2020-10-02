{-# LANGUAGE LambdaCase #-}
{-
The type checker checks if types are valid and does type inference.
-}
module TypeChecker where

import Data.Bifunctor
import Interpreter
import Util
import AST

data TypeError = TypeError {
  _errpos   :: Pos,
  _expected :: Type,
  _actual   :: Type
} deriving Show -- TODO own instance

type TEnv = [(String, Type)]

check :: File -> Result String File
check file = mapM (checkDecl file env) file >>=
  \d's -> if and (map snd d's)
          then check (map fst d's)
          else Success (map fst d's)
  where env = map (\d -> (_name d, _dtype d)) file

-- Returns wheter any types have been infered.
checkDecl :: File -> TEnv -> Decl -> Result String (Decl, Bool)
checkDecl f env d = first show $
                    checkType f env  (_dtype d) >>= \(type', env', changed_t) ->
                    checkExpr f env' (Expr (_body d) (_dtype d))  >>= \(body', changed_b) ->
                    Success (Decl (_name d) (_pos d) type' (_eval body'), changed_t && changed_b)
  
checkType :: File -> TEnv -> Type -> Result String (Type, TEnv, Bool)
checkType f e (TFn a b) = checkType f e a >>= \(a', e', changed_a) ->
                          checkType f e' b >>= \(b', e'', changed_b) ->
                          Success (TFn a' b', e'', changed_a || changed_b)
checkType f e (TNamed s t) = checkType f e t >>= \(t', e', changed) ->
                             Success (t', (s, t):e', changed)
checkType _ e expr@(TExpr (Expr (EVar s) _)) = case lookup s e of
                                                 Just t  -> Success (t, e, True)
                                                 Nothing -> (if s `elem` builtinTypes
                                                             then Success (expr, e, False)
                                                             else Error $ "Type " ++ s ++ " not found")
checkType f e (TExpr expr) = interpret f expr >>= \expr' ->
                             Success (TExpr expr', e, expr /= expr')
checkType _ e t = Success (t, e, False)

checkExpr :: File -> TEnv -> Expr -> Result String (Expr, Bool)
checkExpr f e (Expr (EAp (Expr a ta) (Expr b tb)) t) = combine f ta (TFn tb TAny) >>= \case -- TODO very ugly! bad coder!
  (ta'@(TFn _ t'), changed_ta) -> (combine f t t' >>= \(t'', changed_t) ->
    case a of
      (ELambda s x) -> checkExpr f ((s, tb):e) (Expr a ta') >>= \(a', changed_a) ->
        combine f ta' (_type a') >>= \(ta'', changed_ta') ->
        checkExpr f e (Expr b tb) >>= \(b', changed_b) ->
          Success (Expr (EAp (Expr (_eval a') ta'') b') t'', changed_ta || changed_ta' || changed_t || changed_a || changed_b)
      _ -> Error "Expression doesn't match lambda type")
  _ -> Error "How is this possible? :O"
checkExpr f e (Expr (ELambda s (Expr b tb)) tx) = undefined
                                                       
                                                     

combine :: File -> Type -> Type -> Result String (Type, Bool)
combine f (TNamed _ a) b            = combine f a b
combine f a            (TNamed _ b) = combine f a b
combine _ TAny         b            = Success (b, True)
combine _ a            TAny         = Success (a, True)
combine f (TFn a b)    (TFn a' b')  = combine f a a' >>= \(a'', ca) ->
                                      combine f b b' >>= \(b'', cb) ->
                                      Success (TFn a'' b'', ca || cb)
combine _ TType        TType        = Success (TType, False)
combine f (TExpr e)    b            = interpret f e >>= toType >>= \a -> combine f a b 
combine f a            (TExpr e)    = interpret f e >>= toType >>= \b -> combine f a b 
combine _ a            b            = Error $ "Cannot combine type " ++ show a ++ " with type " ++ show b 

toType :: Expr -> Result String Type
toType (Expr (EType t) _) = Success t
toType _                  = Error "Expression is not of type type"
