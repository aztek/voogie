module Kyckling.TPTP.Pretty (prettyTPTP) where

import Data.List

import Kyckling.TPTP

list :: [String] -> String
list = intercalate ", "

prettyFun :: Fun -> String
prettyFun f =
  case f of
    Sum        -> "$sum"
    Difference -> "$difference"
    Product    -> "$product"
    Divide     -> "$divide"
    Uminus     -> "$uminus"
    Lesseq     -> "$lesseq"
    Less       -> "$less"
    Greatereq  -> "$greatereq"
    Greater    -> "$greater"
    Select     -> "$select"
    Store      -> "$store"

prettyBinaryOp :: BinaryOp -> String
prettyBinaryOp op =
  case op of 
    Impl -> "=>"
    Xor  -> "<~>"
    Iff  -> "<=>"
    And  -> "&"
    Or   -> "|"

prettyUnaryOp :: UnaryOp -> String
prettyUnaryOp op =
  case op of
    Not -> "~"

prettySort :: Sort -> String
prettySort s =
  case s of
    Individual -> "$i"
    Boolean    -> "$o"
    Integer    -> "$int"
    STuple ss  -> "[" ++ list (map prettySort ss) ++ "]"
    Array i t  -> "$array(" ++ list [prettySort i, prettySort t] ++ ")"

prettySortedVar :: SortedVar -> String
prettySortedVar (SortedVar v s) = v ++ ":" ++ prettySort s

prettyDefinition :: Definition -> String
prettyDefinition (Symbol f []) = prettyFun f
prettyDefinition (Symbol f vs) = prettyFun f ++ "(" ++ list (map prettySortedVar vs) ++ ")"

prettyBinding :: Binding -> String
prettyBinding (Binding d t) = prettyDefinition d ++ " := " ++ prettyTerm t

prettyQuantifier :: Quantifier -> String
prettyQuantifier q =
  case q of
    Forall -> "!"
    Exists -> "?"

prettyTerm :: Term -> String
prettyTerm (IntegerConst i) = show i
prettyTerm (BooleanConst True) = "$true"
prettyTerm (BooleanConst False) = "$false"
prettyTerm (FunApp f []) = prettyFun f
prettyTerm (FunApp f ts) = prettyFun f ++ "(" ++ list (map prettyTerm ts) ++ ")"
prettyTerm (Var v) = v
prettyTerm (Binary op a b) = prettyTerm a ++ " " ++ prettyBinaryOp op ++ " " ++ prettyTerm b
prettyTerm (Quantify q [] t) = prettyTerm t
prettyTerm (Quantify q vs t) = prettyQuantifier q ++ " [" ++ list (map prettySortedVar vs) ++ "]: " ++ prettyTerm t
prettyTerm (Let []  t) = prettyTerm t
prettyTerm (Let [b] t) = "$let(" ++ list [prettyBinding b, prettyTerm t] ++ ")"
prettyTerm (Let bs  t) = "$let([" ++ list (map prettyBinding bs) ++ "], " ++ prettyTerm t ++ ")"
prettyTerm (If c a b) = "$ite(" ++ list [prettyTerm c, prettyTerm a, prettyTerm b] ++ ")"

prettyInputType :: InputType -> String
prettyInputType it =
  case it of
    Axiom      -> "axiom"
    Conjecture -> "conjecture"

prettyTPTP :: TPTP -> String
prettyTPTP (TPTP name it t) = "thf(" ++ list [name, prettyInputType it, prettyTerm t] ++ ")."