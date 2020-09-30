module Lib (
  File, Symbol, Decl, Type, Expr, Eval,
  parse
) where

import AST
import Parser
import Util
import Data.Bifunctor
import Control.Applicative

parse :: String -> Either String Decl
parse s = bimap snd (\(_, _, f) -> f) $ toEither $ parseSrc declaration s
