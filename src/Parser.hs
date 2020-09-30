{-# LANGUAGE LambdaCase #-}
module Parser where

import Data.Bifunctor
import Control.Applicative
import Util
import AST

type Error = String

type ParseError = (Pos, Error)

parse :: String -> Result ParseError File
parse s = second (\(_, _, f) -> f) $ parseSrc file s
  
file :: Parser String Error File
file = many declaration <* eof

-- TODO support infix expressions
declaration :: Parser String Error Decl
declaration = (\s t e -> Decl s 0 t e) <$> decl_name <*> ptype <*> (symbol "=" *> expr)
  where
    decl_name = symbol "::" *> str "'" *> some (noneOf "()\\ \n\t'") <* symbol "'"

ptype :: Parser String Error Type
ptype = (\x xs -> mkApChain TFn (x:xs)) <$> typeNoFn <*> many (symbol "->" *> typeNoFn)

typeNoFn :: Parser String Error Type
typeNoFn = TNamed <$> some (noneOf "()\\ \n\t:") <*> (str ":" *> typeSimple)
       <|> TExpr <$> exprNAp
       <|> TExpr <$> (symbol "(" *> expr <* symbol ")")
       <|> typeSimple

-- Types that are not context dependent
typeSimple :: Parser String Error Type
typeSimple = TType <$ symbol "*"
         <|> TAny <$ symbol "_"
         <|> TExpr . (\x -> Expr (EVar x) TType) <$> any_builtin
         <|> symbol "(" *> ptype <* symbol ")"
  where
    any_builtin = foldl (\a b -> a <|> symbol b) (Parser (\_ -> parseError "Not a builtin type")) builtinTypes
  
expr :: Parser String Error Expr
expr = mkApChain mkExpr <$> some exprNAp
  where mkExpr a b = Expr (EAp a b) (TFn (_type a) (_type b))

exprNAp :: Parser String Error Expr
exprNAp = symbol "(" *> expr <* symbol ")"
      <|> str "λ" *> lambda
      <|> mk_builtin EStr "String"     <$> string
      <|> mk_builtin EChar "Char"      <$> char
      <|> mk_builtin EByte "Byte"      <$> byte
      <|> mk_builtin EPtr "&"          <$> ptr
      <|> mk_builtin EF64 "F64" . read <$> float
      <|> mk_builtin EI64 "I64" . read <$> some any_digit
      <|> (\x -> Expr (EType x) TType) <$> typeSimple
      <|> (\x -> Expr (EVar x) TAny)   <$> anySymbol
  where
    mk_builtin ef s x = Expr (ef x) (TExpr (Expr (EVar s) TType))
    any_digit = anyOf "0123456789"
    float  = (\a b c -> a ++ b ++ c) <$> many any_digit <*> str "." <*> some any_digit
    string = str "\"" *> many (noneOf "\"" <|> ('"' <$ str "\\\"")) <* str "\""
    char   = anyOf "'" *> (noneOf "'" <|> ('\'' <$ str "\\'")) <* anyOf "'"
    byte   = str "0x" *> ((\a b -> a*16 + b) <$> anyHex <*> anyHex)
    ptr    = foldl (\a b -> a*16 + b) 0 <$> (str "&" *> some anyHex)

lambda :: Parser String Error Expr
lambda = mkLambda <$> some (noneOf "()\\ \n\t.") <* str "." <*> lambda
     <|> exprNAp
  where mkLambda s e = Expr (ELambda s e) (TFn TAny (_type e))
  
str :: String -> Parser String Error String
str s = Parser $ \i -> if take len i == s then
                         Success (drop len i, posOffsetOf s, s)
                       else
                         parseError ("Input does not match '" ++ s ++ "'")
  where len = length s

symbol :: String -> Parser String Error String
symbol s = str s <* many ws

noneOf :: [Char] -> Parser String Error Char
noneOf vs = Parser $ \case
  []     -> parseError "Empty input"
  (x:_) | elem x vs -> parseError $ "Should not match '" ++ [x] ++ "'"
  (x:xs) -> Success (xs, posOffsetOf [x], x)

anyOf :: [Char] -> Parser String Error Char
anyOf vs = Parser $ \case
  []     -> parseError "Empty input"
  (x:xs) | elem x vs -> Success (xs, posOffsetOf [x], x)
  (_:_)              -> parseError $ "Input does not match any of '" ++ vs ++ "'"

anySymbol :: Parser String Error String
anySymbol = some (noneOf "()\\ \n\t") <* optional ws

anyHex :: (Integral a, Read a) => Parser String Error a
anyHex = read . pure <$> anyOf "0123456789"
     <|> 10 <$ anyOf "aA"
     <|> 11 <$ anyOf "bB"
     <|> 12 <$ anyOf "cC"
     <|> 13 <$ anyOf "dD"
     <|> 14 <$ anyOf "eE"
     <|> 15 <$ anyOf "fF"
  
ws :: Parser String Error String
ws = str "\n" <|> str "\t" <|> str " "

eof :: Parser String Error ()
eof = Parser $ \i -> if null i then
                       Success ("", Pos 0 0, ())
                     else
                       Error (Pos 0 0, "There is still input left: " ++ shorten i)
  where shorten s = take 16 s ++ if length s > 16 then "..." else ""  

-- TODO not efficient
mkApChain :: (a -> a -> a) -> [a] -> a
mkApChain _ []   = error "Empty expression"
mkApChain _ [x]  = x
mkApChain f xs = f (mkApChain f $ init xs) (last xs)

parseError :: String -> Result (Pos, String) a
parseError s = Error (Pos 0 0, s)
