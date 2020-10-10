module IR where

import           Util
import           Data.Word
import           Interpreter

type File = [Expr]

type Expr = (Eval, Type)

data Type = TFn Type Type
          | TEither Type Type
          | TNamed Symbol Type
          | TType
          | TAny
          | TExpr Expr
          | TToken Symbol
  deriving (Eq, Show) -- TODO own instances

data Eval = EAp Expr Expr
          | ELambda Symbol Expr
          | EType Type
          | EI64 Int
          | EF64 Float
          | EStr String
          | EChar Char
          | EByte Word8
          | EPtr Int
          | ELocalRef Int
          | ECall { _decl :: Int, _nargs :: Int }
  deriving (Eq, Show) -- TODO own instances

data TypeError = TypeError {
  _errpos   :: Pos,
  _expected :: Type,
  _actual   :: Type
} deriving Show -- TODO own instance

combine :: File -> Type -> Type -> Result String (Bool, Type)
combine f (TNamed _ a) b            = combine f a b
combine f a            (TNamed _ b) = combine f a b
combine _ TAny         b            = Success (True, b)
combine _ a            TAny         = Success (True, a)
combine f (TFn a b)    (TFn a' b')  = combine f a a' >>= \(ca, a'') ->
  combine f b b' >>= \(cb, b'') -> Success (ca || cb, TFn a'' b'')
combine _ TType     TType     = Success (False, TType)
combine f (TExpr e) b         = interpret f e >>= toType >>= \a -> combine f a b
combine f a         (TExpr e) = interpret f e >>= toType >>= \b -> combine f a b
combine _ a b =
  Error $ "Cannot combine type " ++ show a ++ " with type " ++ show b

combineNoExpr :: Type -> Type -> Result String (Bool, Type)
combineNoExpr (TNamed _ a) b            = combineNoExpr a b
combineNoExpr a            (TNamed _ b) = combineNoExpr a b
combineNoExpr TAny         b            = Success (True, b)
combineNoExpr a            TAny         = Success (True, a)
combineNoExpr (TFn a b)    (TFn a' b')  = combineNoExpr a a' >>= \(ca, a'') ->
  combineNoExpr b b' >>= \(cb, b'') -> Success (ca || cb, TFn a'' b'')
combineNoExpr TType     TType     = Success (False, TType)
combineNoExpr (TExpr e) _         = Success (False, TExpr e)
combineNoExpr _         (TExpr e) = Success (False, TExpr e)
combineNoExpr a b =
  Error $ "Cannot combine type " ++ show a ++ " with type " ++ show b

toType :: Expr -> Result String Type
toType ((EType t), _) = Success t
toType _              = Error "Expression is not of type type"
