module Voogie.FOOL.Pretty (
  pretty
) where

import qualified Voogie.NonEmpty as VNE

import Voogie.Theory
import Voogie.Pretty
import Voogie.FOOL

instance Pretty Term where
  pretty (IntegerConstant i) = pretty i
  pretty (BooleanConstant b) = pretty b
  pretty (Variable (Typed _ (Var v))) = v
  pretty (Constant (Typed _ f)) = f
  pretty (Application (Typed _ f) args) = f ++ parens (VNE.intercalate ", " $ fmap pretty args)
  pretty (Binary op a b) = unwords [pretty a, pretty op, pretty b]
  pretty (Unary op t) = pretty op ++ pretty t
  pretty (Quantify q vars t) = pretty q ++ " " ++ parens (VNE.intercalate ", " $ fmap p vars) ++ pretty t
    where p (Typed t (Var v)) = pretty t ++ " " ++ v
  pretty (Equals s a b) = unwords [pretty a, pretty s, pretty b]
  pretty (If c a b) = unwords [pretty c, "?", pretty a, ":", pretty b]
  pretty (Select a i) = pretty a ++ brackets (pretty i)
  pretty (Store a i v) = unwords [pretty a ++ brackets (pretty i), ":=", pretty v]
  pretty t = error $ "no pretty syntax for " ++ show t
