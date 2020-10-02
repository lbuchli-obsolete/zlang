module AST where

import Data.Word
import Util

{-
A file
-}
type File = [Decl]

-- A String without whitespace
type Symbol = String

data Decl = Decl {
  _name :: Symbol,
  _pos :: Int,
  _dtype :: Type,
  _body :: Eval
} deriving Show -- TODO own show instance

data Type = TFn Type Type
          | TNamed Symbol Type
          | TType
          | TAny
          | TExpr Expr -- An expr returning a type
  deriving (Eq, Show) -- TODO own eq (evaluate TExpr, TNamed -> Type) and show instance

data Expr = Expr {
  _eval :: Eval,
  _type :: Type
} deriving (Eq, Show) -- TODO own show instance 

data Eval = EAp Expr Expr
          | ELambda Symbol Expr
          | EType Type
          | EI64 Int
          | EF64 Float
          | EStr String
          | EChar Char
          | EByte Word8
          | EPtr Int
          | EVar Symbol
  deriving (Eq, Show) -- TODO own eq (alpha equivalence) and show instance 

guessType :: Eval -> Result String Type
guessType (EVar x) | x `elem` builtinTypes = Success TType
guessType (EVar _)                         = Success TAny
guessType (EType _)                        = Success TType
guessType (EI64 _)                         = Success $ builtinType 0
guessType (EF64 _)                         = Success $ builtinType 1
guessType (EStr _)                         = Success $ builtinType 2
guessType (EChar _)                        = Success $ builtinType 3
guessType (EByte _)                        = Success $ builtinType 4
guessType (EPtr _)                         = Success $ builtinType 5
guessType (ELambda _ expr)                 = Success $ TFn TAny (_type expr)
guessType (EAp a b)                        = combine_types (_type a) (_type b)
  where
    combine_types (TFn from to) tb | from == tb = Success to
    combine_types (TFn from _)  tb              = Error ("Cannot match type " ++ show from ++ " with type " ++ show tb)
    combine_types other         tb              = Error ("Cannot apply type " ++ show tb ++ " to type " ++ show other ++ " (not a function)")

builtinTypes :: [Symbol]
builtinTypes = ["I64", "F64", "String", "Char", "Byte", "&"]

builtinType :: Int -> Type
builtinType x = TExpr (Expr (EVar (builtinTypes !! x)) TType)
