module AST where

import           Util
import           Data.Word

type File = [Decl]

type Decl = ([Tag], Expr)

data Tag = Priority Int
         | Assoc Assoc

data Assoc = AssocLeft | AssocRight

data Type = TFn [Type]
          | TEither Type Type
          | TNamed Symbol Type
          | TType
          | TAny
          | TExpr Expr -- An expr returning a type
          | TI64 | TF64 | TList Type | TChar | TByte
          | TToken Symbol
  deriving (Eq, Show) -- TODO own eq (evaluate TExpr, TNamed -> Type) and show instance

type Expr = (Eval, Type)

data Eval = EAp [Expr]
          | ELambda Symbol Expr
          | EType Type
          | EI64 Int
          | EF64 Float
          | EList [Expr]
          | EChar Char
          | EByte Word8
          | EVar Symbol
  deriving (Eq, Show) -- TODO own eq (alpha equivalence) and show instance

guessType :: Eval -> Type
guessType (EVar x) | x `elem` builtinTypes = TType
guessType (EVar  _           )             = TAny
guessType (EType _           )             = TType
guessType (EI64  _           )             = TI64
guessType (EF64  _           )             = TF64
guessType (EList ((_, t) : _))             = TList t
guessType (EList []          )             = TList TAny
guessType (EChar _           )             = TChar
guessType (EByte _           )             = TByte
guessType (ELambda _ e       )             = TFn [TAny, (snd e)]
guessType (EAp _             )             = TAny

builtinTypes :: [Symbol]
builtinTypes = ["I64", "F64", "[]", "Char", "Byte"]
