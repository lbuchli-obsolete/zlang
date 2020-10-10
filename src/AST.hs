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
          | TToken Symbol
  deriving (Eq, Show) -- TODO own eq (evaluate TExpr, TNamed -> Type) and show instance

-- data Expr = Expr {
--   _eval :: Eval,
--   _type :: Type
-- } deriving (Eq, Show) -- TODO own show instance

type Expr = (Eval, Type)

data Eval = EAp [Expr]
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
guessType (EVar  _    )                    = Success TAny
guessType (EType _    )                    = Success TType
guessType (EI64  _    )                    = Success $ builtinType 0
guessType (EF64  _    )                    = Success $ builtinType 1
guessType (EStr  _    )                    = Success $ builtinType 2
guessType (EChar _    )                    = Success $ builtinType 3
guessType (EByte _    )                    = Success $ builtinType 4
guessType (EPtr  _    )                    = Success $ builtinType 5
guessType (ELambda _ e)                    = Success $ TFn [TAny, (snd e)]
guessType (EAp _      )                    = Success TAny

builtinTypes :: [Symbol]
builtinTypes = ["I64", "F64", "String", "Char", "Byte", "&"]

builtinType :: Int -> Type
builtinType x = TExpr (EVar (builtinTypes !! x), TType)
