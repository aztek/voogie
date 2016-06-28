module Kyckling.FOOL.Pretty (
  prettyType,
  prettyUnaryOp,
  prettyBinaryOp,
  prettyTerm,
  prettyFormula
) where

import Data.List

import Kyckling.Theory
import Kyckling.FOOL

prettyQuantifier :: Quantifier -> String
prettyQuantifier Forall = "forall"
prettyQuantifier Exists = "exists"

prettyType :: Type -> String
prettyType Integer = "int"
prettyType Boolean = "bool"
prettyType (Array t) = prettyType t ++ "[]"

prettyUnaryOp :: UnaryOp -> String
prettyUnaryOp Negate   = "!"
prettyUnaryOp Positive = "+"
prettyUnaryOp Negative = "-"

prettyBinaryOp :: BinaryOp -> String
prettyBinaryOp op =
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

prettyTerm :: Term -> String
prettyTerm (IntegerConst i) = show i
prettyTerm (BooleanConst b) = if b then "true" else "false"
prettyTerm (Variable (Typed (Var v) _)) = v
prettyTerm (Const (Typed c _)) = c
prettyTerm (Binary op a b) = prettyTerm a ++ " " ++ prettyBinaryOp op ++ " " ++ prettyTerm b
prettyTerm (Unary op t) = prettyUnaryOp op ++ prettyTerm t
prettyTerm (Quantify q vars t) = prettyQuantifier q ++ " (" ++ intercalate ", " (map pretty vars) ++ ")" ++ prettyTerm t
  where pretty (Typed (Var v) t) = prettyType t ++ " " ++ v
prettyTerm (Eql   a b) = prettyTerm a ++ " == " ++ prettyTerm b
prettyTerm (InEql a b) = prettyTerm a ++ " != " ++ prettyTerm b
prettyTerm (If c a b) = prettyTerm c ++ " ? " ++ prettyTerm a ++ " : " ++ prettyTerm b
prettyTerm t = error $ "no pretty syntax for " ++ show t

prettyFormula = prettyTerm