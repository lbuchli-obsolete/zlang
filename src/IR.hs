module IR where

import           Util
import           Data.Word

type File = [Expr]

type Expr = (Eval, Type)

data Type = TFn Type Type
          | TEither Type Type
          | TNamed Symbol Type
          | TType
          | TAny
          | TExpr Expr
          | TI64 | TF64 | TList Type | TChar | TByte
  deriving (Eq, Show) -- TODO own instances

data Eval = EAp Expr Expr
          | ELambda Symbol Expr
          | EType Type
          | EI64 Int
          | EF64 Float
          | EList [Expr]
          | EChar Char
          | EByte Word8
          | ELocalRef Int
          | ECall { _decl :: Int, _nargs :: Int }
  deriving (Eq, Show) -- TODO own instances

data TypeError = TypeError {
  _errpos   :: Pos,
  _expected :: Type,
  _actual   :: Type
} deriving Show -- TODO own instance

guessType :: Eval -> Type
guessType (EAp     _ _       ) = TAny -- TODO better detection?
guessType (ELambda _ (_, t)  ) = TFn TAny t
guessType (EType _           ) = TType
guessType (EI64  _           ) = TI64
guessType (EF64  _           ) = TF64
guessType (EList ((_, t) : _)) = TList t
guessType (EList []          ) = TList TAny
guessType (EChar _           ) = TChar
guessType (EByte _           ) = TByte
guessType _                    = TAny
