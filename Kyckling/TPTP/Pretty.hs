module Kyckling.TPTP.Pretty (showTPTP) where

import Data.List

import Kyckling.TPTP

list :: [String] -> String
list = intercalate ", "

showFun :: Fun -> String
showFun f =
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

showBinaryOp :: BinaryOp -> String
showBinaryOp op =
  case op of 
    Impl -> "=>"
    Xor  -> "<~>"
    Iff  -> "<=>"
    And  -> "&"
    Or   -> "|"

showUnaryOp :: UnaryOp -> String
showUnaryOp op =
  case op of
    Not -> "~"

showSort :: Sort -> String
showSort s =
  case s of
    Individual -> "$i"
    Boolean    -> "$o"
    Integer    -> "$int"
    STuple ss  -> "[" ++ list (map showSort ss) ++ "]"
    Array i t  -> "$array(" ++ list [showSort i, showSort t] ++ ")"

showSortedVar :: SortedVar -> String
showSortedVar (SortedVar v s) = v ++ ":" ++ showSort s

showDefinition :: Definition -> String
showDefinition (Symbol f []) = showFun f
showDefinition (Symbol f vs) = showFun f ++ "(" ++ list (map showSortedVar vs) ++ ")"

showBinding :: Binding -> String
showBinding (Binding d t) = showDefinition d ++ " := " ++ showTerm t

showQuantifier :: Quantifier -> String
showQuantifier q =
  case q of
    Forall -> "!"
    Exists -> "?"

showTerm :: Term -> String
showTerm (FunApp f []) = showFun f
showTerm (FunApp f ts) = showFun f ++ "(" ++ list (map showTerm ts) ++ ")"
showTerm (Var v) = v
showTerm (Binary op a b) = showTerm a ++ " " ++ showBinaryOp op ++ " " ++ showTerm b
showTerm (Quantify q [] t) = showTerm t
showTerm (Quantify q vs t) = showQuantifier q ++ " [" ++ list (map showSortedVar vs) ++ "]: " ++ showTerm t
showTerm (Let []  t) = showTerm t
showTerm (Let [b] t) = "$let(" ++ list [showBinding b, showTerm t] ++ ")"
showTerm (Let bs  t) = "$let([" ++ list (map showBinding bs) ++ "], " ++ showTerm t ++ ")"
showTerm (If c a b) = "$ite(" ++ list [showTerm c, showTerm a, showTerm b] ++ ")"

showInputType :: InputType -> String
showInputType it =
  case it of
    Axiom      -> "axiom"
    Conjecture -> "conjecture"

showTPTP :: TPTP -> String
showTPTP (TPTP name it t) = "thf(" ++ list [name, showInputType it, showTerm t] ++ ")."