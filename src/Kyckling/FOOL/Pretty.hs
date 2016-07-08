module Kyckling.FOOL.Pretty (
  pretty
) where

import Data.List

import Kyckling.Theory
import Kyckling.Pretty
import Kyckling.FOOL

instance Pretty Quantifier where
  pretty Forall = "forall"
  pretty Exists = "exists"

instance Pretty Type where
  pretty Integer = "int"
  pretty Boolean = "bool"
  pretty (Array t) = pretty t ++ "[]"
  pretty (TupleType ts) = "(" ++ intercalate ", " (map pretty ts) ++ ")"
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

instance Pretty Term where
  pretty (IntegerConst i) = show i
  pretty (BooleanConst b) = if b then "true" else "false"
  pretty (Variable (Typed (Var v) _)) = v
  pretty (Const (Typed c _)) = c
  pretty (Binary op a b) = pretty a ++ " " ++ pretty op ++ " " ++ pretty b
  pretty (Unary op t) = pretty op ++ pretty t
  pretty (Quantify q vars t) = pretty q ++ " (" ++ intercalate ", " (map p vars) ++ ")" ++ pretty t
    where p (Typed (Var v) t) = pretty t ++ " " ++ v
  pretty (Eql   a b) = pretty a ++ " == " ++ pretty b
  pretty (InEql a b) = pretty a ++ " != " ++ pretty b
  pretty (If  c a b) = pretty c ++ " ? "  ++ pretty a ++ " : " ++ pretty b
  pretty t = error $ "no pretty syntax for " ++ show t