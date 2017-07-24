module Voogie.FOOL.Pretty (
  pretty
) where

import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Voogie.NonEmpty as VNE

import Voogie.Theory
import Voogie.Pretty
import Voogie.FOOL
import qualified Voogie.FOOL.Tuple as Tuple

instance Pretty Quantifier where
  pretty Forall = "forall"
  pretty Exists = "exists"

instance Pretty Type where
  pretty Integer = "int"
  pretty Boolean = "bool"
  pretty (Array i t) = brackets (VNE.intercalate ", " $ fmap pretty i) ++ " " ++ pretty t
  pretty (TupleType ts) = parens (Tuple.intercalate ", " $ fmap pretty ts)

instance Pretty UnaryOp where
  pretty Negate   = "!"
  pretty Positive = "+"
  pretty Negative = "-"

instance Pretty BinaryOp where
  pretty op =
    case op of
      And      -> "&&"
      Or       -> "||"
      Imply    -> "=>"
      Iff      -> "=="
      Xor      -> "!="
      Greater  -> ">"
      Less     -> "<"
      Geq      -> ">="
      Leq      -> "<="
      Add      -> "+"
      Subtract -> "-"
      Multiply -> "*"
      Divide   -> "/"

instance Pretty Sign where
  pretty Pos = "=="
  pretty Neg = "!="

instance Pretty Term where
  pretty (IntegerConstant i) = show i
  pretty (BooleanConstant True)  = "true"
  pretty (BooleanConstant False) = "false"
  pretty (Variable (Typed (Var v) _)) = v
  pretty (Constant (Typed f _)) = f
  pretty (Application (Typed f _) args) = f ++ parens (VNE.intercalate ", " $ fmap pretty args)
  pretty (Binary op a b) = unwords [pretty a, pretty op, pretty b]
  pretty (Unary op t) = pretty op ++ pretty t
  pretty (Quantify q vars t) = pretty q ++ " " ++ parens (VNE.intercalate ", " $ fmap p vars) ++ pretty t
    where p (Typed (Var v) t) = pretty t ++ " " ++ v
  pretty (Equals s a b) = unwords [pretty a, pretty s, pretty b]
  pretty (If c a b) = unwords [pretty c, "?", pretty a, ":", pretty b]
  pretty (Select a i) = pretty a ++ brackets (pretty i)
  pretty (Store a i v) = pretty a ++ brackets (pretty i) ++ " := " ++ pretty v
  pretty t = error $ "no pretty syntax for " ++ show t
