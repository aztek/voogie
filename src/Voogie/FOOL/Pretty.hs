module Voogie.FOOL.Pretty (
  pretty
) where

import Data.List

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
  pretty (Array i t) = "[" ++ pretty i ++ "] " ++ pretty t
  pretty (TupleType ts) = "(" ++ Tuple.intercalate ", " (fmap pretty ts) ++ ")"
  pretty (OptionType t) = "option(" ++ pretty t ++ ")"
  pretty (EitherType l r) = "either(" ++ pretty l ++ ", " ++ pretty r ++ ")"

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

instance Pretty Term where
  pretty (IntegerConstant i) = show i
  pretty (BooleanConstant b) = if b then "true" else "false"
  pretty (Variable (Typed (Var v) _)) = v
  pretty (Application (Typed f _) args) = f ++ "(" ++ intercalate ", " (map pretty args) ++ ")"
  pretty (Binary op a b) = pretty a ++ " " ++ pretty op ++ " " ++ pretty b
  pretty (Unary op t) = pretty op ++ pretty t
  pretty (Quantify q vars t) = pretty q ++ " (" ++ intercalate ", " (map p vars) ++ ")" ++ pretty t
    where p (Typed (Var v) t) = pretty t ++ " " ++ v
  pretty (Equals s a b) = pretty a ++ (if s == Pos then " == " else " != ") ++ pretty b
  pretty (If c a b) = pretty c ++ " ? "  ++ pretty a ++ " : " ++ pretty b
  pretty (Select a i) = pretty a ++ "[" ++ pretty i ++ "]"
  pretty t = error $ "no pretty syntax for " ++ show t
