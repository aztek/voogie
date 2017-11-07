module Voogie.FOOL.Pretty (pretty) where

import qualified Voogie.NonEmpty as VNE

import Voogie.Pretty
import Voogie.FOOL

instance Pretty Var where
  pretty (Var v) = v

instance Pretty Term where
  pretty t = case t of
    IntegerConstant i -> pretty i
    BooleanConstant b -> pretty b
    Variable v -> pretty v
    Constant f -> pretty f
    Application f as -> pretty f ++ parens args
      where args = VNE.intercalate ", " (fmap pretty as)
    Binary op a b -> unwords [pretty a, pretty op, pretty b]
    Unary op t -> pretty op ++ pretty t
    Quantify q vs t -> unwords [pretty q, parens vars ++ pretty t]
      where vars = VNE.intercalate ", " (fmap prettyTyped vs)
    Equals s a b -> unwords [pretty a, pretty s, pretty b]
    If c a b -> unwords [pretty c, "?", pretty a, ":", pretty b]
    Select a i -> pretty a ++ brackets (pretty i)
    Store a i v -> unwords [pretty a ++ brackets (pretty i), ":=", pretty v]
    t -> error $ "no pretty syntax for " ++ show t
