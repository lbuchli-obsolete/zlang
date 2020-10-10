{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
module Parser where

import Data.Bifunctor
import Control.Applicative
import Util
import AST

type ZParser a = Parser String String a

parse :: String -> Result String File
parse s = bimap show (\(_, _, f) -> f) $ parseSrc file s
  
file :: ZParser File
file = many declaration <* eof -- TODO comment lines

declaration :: ZParser Decl
declaration = (,) <$> many tag <*> expr

tag :: ZParser Tag
tag = symbol ":#" *> (priority <|> (Assoc <$> associativity))
  where
    priority = Priority . read <$> (symbol "priority" *> some (anyOf "0123456789"))
    associativity = AssocLeft <$ (symbol "assoc" *> symbol "left")
                <|> AssocRight <$ (symbol "assoc" *> symbol "right")

ptype :: ZParser Type
ptype = (\x xs -> TFn (x:xs)) <$> typeNoFn <*> many (symbol "->" *> typeNoFn)

typeNoFn :: ZParser Type
typeNoFn = TNamed <$> some (noneOf "()\\ \n\t':") <*> (str ":" *> typeSimple)
       <|> TEither <$> typeSimple <*> (symbol "|" *> typeSimple)
       <|> typeSimple

-- Types that are not context dependent
typeSimple :: ZParser Type
typeSimple = TType <$ symbol "*"
         <|> TAny <$ symbol "_"
         <|> TToken <$> (str "'" *> some (noneOf "()\\ \n\t'") <* symbol "'")
         <|> (\s -> TExpr (EVar s, TAny)) <$> anySymbol 
         <|> TExpr <$> (str "$" *> exprNAp)
         <|> symbol "(" *> ptype <* symbol ")"
  
expr :: ZParser Expr
expr = (\t e -> (fst e, t)) <$> (symbol "::" *> ptype <* symbol "=") <*> chain
   <|> chain
  where
    chain = (\exprs -> (EAp exprs, TAny)) <$> some exprNAp 

exprNAp :: ZParser Expr
exprNAp = symbol "(" *> expr <* symbol ")"
      <|> str "λ" *> lambda
      <|> mk_builtin EStr "String"     <$> string
      <|> mk_builtin EChar "Char"      <$> char
      <|> mk_builtin EByte "Byte"      <$> byte
      <|> mk_builtin EPtr "&"          <$> ptr
      <|> mk_builtin EF64 "F64" . read <$> float
      <|> mk_builtin EI64 "I64" . read <$> some any_digit
      <|> (\x -> (EType x, TType))     <$> typeSimple
      <|> (\x -> (EVar x, TAny))       <$> anySymbol
  where
    mk_builtin ef s x = (ef x, (TExpr (EVar s, TType)))
    any_digit = anyOf "0123456789"
    float  = (\a b c -> a ++ b ++ c) <$> many any_digit <*> str "." <*> some any_digit
    string = str "\"" *> many (noneOf "\"" <|> ('"' <$ str "\\\"")) <* str "\""
    char   = anyOf "'" *> (noneOf "'" <|> ('\'' <$ str "\\'")) <* anyOf "'"
    byte   = str "0x" *> ((\a b -> a*16 + b) <$> anyHex <*> anyHex)
    ptr    = foldl (\a b -> a*16 + b) 0 <$> (str "&" *> some anyHex)

lambda :: ZParser Expr
lambda = mkLambda <$> some (noneOf "()\\ \n\t'.") <* str "." <*> lambda
     <|> exprNAp
  where mkLambda s e = (ELambda s e, (TFn [TAny, (snd e)]))
  
str :: String -> ZParser String
str s = Parser $ \i -> if take len i == s then
                         Success (drop len i, posOffsetOf s, s)
                       else
                         parseError ("Input does not match '" ++ s ++ "'")
  where len = length s

symbol :: String -> ZParser String
symbol s = str s <* many ws

noneOf :: [Char] -> ZParser Char
noneOf vs = Parser $ \case
  []     -> parseError "Empty input"
  (x:_) | elem x vs -> parseError $ "Should not match '" ++ [x] ++ "'"
  (x:xs) -> Success (xs, posOffsetOf [x], x)

anyOf :: [Char] -> ZParser Char
anyOf vs = Parser $ \case
  []     -> parseError "Empty input"
  (x:xs) | elem x vs -> Success (xs, posOffsetOf [x], x)
  (_:_)              -> parseError $ "Input does not match any of '" ++ vs ++ "'"

anySymbol :: ZParser String
anySymbol = some (noneOf "()\\ \n\t'") <* optional ws

anyHex :: (Integral a, Read a) => ZParser a
anyHex = read . pure <$> anyOf "0123456789"
     <|> 10 <$ anyOf "aA"
     <|> 11 <$ anyOf "bB"
     <|> 12 <$ anyOf "cC"
     <|> 13 <$ anyOf "dD"
     <|> 14 <$ anyOf "eE"
     <|> 15 <$ anyOf "fF"

{-
TODO group on equal or less indentation, e.g.
a
  b c
  d e
    f
= a (b c) (d e f)
-}
ws :: ZParser String
ws = str "\n" <|> str "\t" <|> str " "

eof :: ZParser ()
eof = Parser $ \i -> if null i then
                       Success ("", Pos 0 0, ())
                     else
                       Error (Pos 0 0, "There is still input left: " ++ shorten i)
  where shorten s = take 16 s ++ if length s > 16 then "..." else ""  


parseError :: String -> Result (Pos, String) a
parseError s = Error (Pos 0 0, s)
