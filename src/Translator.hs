module Translator where

import           Data.Foldable
import           Data.Maybe
import           Data.List
import           Util
import qualified AST
import qualified IR

type LocalEnv = [(String, AST.Type)]

translate :: AST.File -> Result String IR.File
translate f = mapM (translateExpr f [] . snd) f

translateExpr :: AST.File -> LocalEnv -> AST.Expr -> Result String IR.Expr
translateExpr f env (e, t) = translateType f env t >>= \(t', env') ->
  translateEval f env' e >>= \(e', t'') ->
    IR.combineNoExpr t' t'' >>= \(_, t''') -> Success (e', t''')

translateEval :: AST.File -> LocalEnv -> AST.Eval -> Result String IR.Expr
translateEval f env (AST.EAp exprs     ) = translateAp f env exprs
translateEval f env (AST.ELambda s expr) = translateLambda f env s expr
translateEval f env (AST.EType etype   ) = translateEType f env etype
translateEval _ _   (AST.EI64  _       ) = undefined -- TODO howto builtin types?
translateEval _ _   (AST.EF64  _       ) = undefined
translateEval _ _   (AST.EStr  _       ) = undefined
translateEval _ _   (AST.EChar _       ) = undefined
translateEval _ _   (AST.EByte _       ) = undefined
translateEval _ _   (AST.EPtr  _       ) = undefined
translateEval f env (AST.EVar  v       ) = translateVar f env v

translateAp :: AST.File -> LocalEnv -> [AST.Expr] -> Result String IR.Expr
translateAp f env exs = case bestAp f env exs of
  Just (i, params) -> expand params >>= build_ap i
  Nothing          -> mkApChain construct_ap <$> mapM (translateExpr f env) exs
 where
  expand (x : xs) = case toEither (translateAp f env x) of
    Right expr -> (expr :) <$> expand xs
    Left  _    -> mapM (translateExpr f env) x >>= (<$> expand xs) . (++)
  expand [] = Success []
  build_ap i params = mkApChain construct_ap . (: params) <$> constructCall
    f
    env
    i
    (length params)
  construct_ap a b = (IR.EAp a b, IR.TFn (snd a) (snd b))

translateLambda
  :: AST.File -> LocalEnv -> Symbol -> AST.Expr -> Result String IR.Expr
translateLambda f env s expr =
  (\expr' -> (IR.ELambda s expr', IR.TFn IR.TAny (snd expr')))
    <$> translateExpr f ((s, snd expr) : env) expr

translateEType :: AST.File -> LocalEnv -> AST.Type -> Result String IR.Expr
translateEType f env t =
  (\(t', _) -> (IR.EType t', IR.TType)) <$> translateType f env t

translateType
  :: AST.File -> LocalEnv -> AST.Type -> Result String (IR.Type, LocalEnv)
translateType _ _ (AST.TFn []) = Error "Empty application"
translateType f env (AST.TFn (x : xs)) =
  translateType f env x >>= \x' -> foldrM (foldTypeFn f) x' xs
translateType f env (AST.TEither a b) = -- TODO propagate env?
  (\(a', _) (b', _) -> (IR.TEither a' b', env))
    <$> translateType f env a
    <*> translateType f env b
translateType f env (AST.TNamed s t) =
  (\(t', env') -> (IR.TNamed s t', env')) <$> translateType f env t -- TODO recursive types?
translateType _ env AST.TType = Success (IR.TType, env)
translateType _ env AST.TAny  = Success (IR.TAny, env)
translateType f env (AST.TExpr e) =
  (\e' -> (e', env)) . IR.TExpr <$> translateExpr f env e
translateType _ env (AST.TToken s) = Success (IR.TToken s, env)

foldTypeFn
  :: AST.File
  -> AST.Type
  -> (IR.Type, LocalEnv)
  -> Result String (IR.Type, LocalEnv)
foldTypeFn f at (t, env) =
  (\(t', env') -> (IR.TFn t' t, env')) <$> translateType f env at

translateVar :: AST.File -> LocalEnv -> Symbol -> Result String IR.Expr
translateVar f env s = case lookup s env of
  Just t  -> (,) <$> local_ref <*> (fst <$> translateType f env t)
  Nothing -> Error $ "Variable not in scope: " ++ s
 where
  local_ref =
    Util.fromMaybe $ IR.ELocalRef . (length env -) <$> elemIndex s (map fst env)

constructCall :: AST.File -> LocalEnv -> Int -> Int -> Result String IR.Expr
constructCall f env i len =
  (,) (IR.ECall i len) . fst <$> translateType f env t
 where
  t        = AST.TFn (take len stripped ++ [AST.TFn (drop len stripped)])
  stripped = strip_tokens (snd . snd $ f !! i)
  strip_tokens (AST.TFn ts)   = filter (not . is_token) ts
  strip_tokens x | is_token x = []
  strip_tokens x              = [x]
  is_token (AST.TToken _   ) = True
  is_token (AST.TNamed _ t') = is_token t'
  is_token _                 = False

bestAp :: AST.File -> LocalEnv -> [AST.Expr] -> Maybe (Int, [[AST.Expr]])
bestAp f env exprs = highestPriority f candidates
 where
  candidates = mapMaybe (\(q, r) -> (\c -> (c, r)) <$> candidate f q)
                        (possibleSubqueries exprs env)

highestPriority :: AST.File -> [(Int, a)] -> Maybe (Int, a)
highestPriority f = listToMaybe . sortOn get_priority -- TODO associativity
 where
  get_priority (i, _) = tags_get_priority $ fst (f !! i)
  tags_get_priority ((AST.Priority x) : _ ) = x
  tags_get_priority (_                : ts) = tags_get_priority ts
  tags_get_priority []                      = 0


possibleSubqueries :: [AST.Expr] -> LocalEnv -> [([Symbol], [[AST.Expr]])]
possibleSubqueries ((AST.EVar x, _) : xs) env | not (x `elem` map fst env) =
  map (\(q, r) -> (x : q, [] : r)) other ++ other
  where other = possibleSubqueries xs env
possibleSubqueries (x : xs) env = map (\(q, r) -> (q, attach_x r)) other
 where
  other = possibleSubqueries xs env
  attach_x xss = (x : (head xss)) : (tail xss)
possibleSubqueries [] _ = [([], [[]])]

candidate :: AST.File -> [Symbol] -> Maybe Int
candidate f q = findIndex ((== q) . query . snd . snd) f

query :: AST.Type -> [Symbol]
query (AST.TFn args) = mapMaybe get_symbol args
 where
  get_symbol (AST.TToken s) = Just s
  get_symbol _              = Nothing
query _ = []

-- TODO not efficient -- implementation using foldr?
mkApChain :: (a -> a -> a) -> [a] -> a
mkApChain _ []  = error "Empty expression"
mkApChain _ [x] = x
mkApChain f xs  = f (mkApChain f $ init xs) (last xs)
