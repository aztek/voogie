module Kyckling.FOOL.TPTPretty (
  prettyTPTP
) where

import Data.List

import Kyckling.Type
import Kyckling.FOOL

list :: [String] -> String
list = intercalate ", "

tuple :: [String] -> String
tuple es = "[" ++ list es ++ "]"

parens :: String -> String
parens s = "(" ++ s ++ ")"

funapp :: String -> [String] -> String
funapp f as = f ++ parens (list as)

prettyFun :: Fun -> String
prettyFun f =
  case f of
    Sum        -> "$sum"
    Difference -> "$difference"
    Product    -> "$product"
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
    And  -> "&"
    Or   -> "|"
    Eq   -> "="
    InEq -> "!="

prettyUnaryOp :: UnaryOp -> String
prettyUnaryOp op =
  case op of
    Not -> "~"

prettyType :: Type -> String
prettyType s =
  case s of
    Boolean -> "$o"
    Integer -> "$int"
    Array t -> funapp "$array" ["$int", prettyType t]

prettyConstant :: Constant -> String
prettyConstant = fst

prettyTypedVar :: TypedVar -> String
prettyTypedVar (TypedVar v s) = v ++ ":" ++ prettyType s

prettyDefinition :: Definition -> String
prettyDefinition (Symbol c []) = prettyConstant c
prettyDefinition (Symbol c vs) = funapp (prettyConstant c) (map prettyTypedVar vs)
prettyDefinition (TupleD es) = tuple (map prettyConstant es)

offsetBinding :: Int -> Binding -> String
offsetBinding o (Binding d t) = d' ++ " := " ++ indentedTerm False 0 (o + length d' + 4) t
  where d' = prettyDefinition d

prettyBinding :: Binding -> String
prettyBinding = offsetBinding 0

prettyQuantifier :: Quantifier -> String
prettyQuantifier q =
  case q of
    Forall -> "!"
    Exists -> "?"

indent :: Int -> String
indent = flip replicate ' '

indentedTerm :: Bool -> Int -> Int -> Term -> String
indentedTerm pp i o t = indent i ++ case t of
  IntegerConst n  -> show n
  BooleanConst b  -> if b then "$true" else "$false"
  FunApp Tuple ts -> tuple (map prettyTerm ts)
  FunApp f []     -> prettyFun f
  FunApp f ts     -> funapp (prettyFun f) (map prettyTerm ts)
  Var v           -> v
  Const c         -> prettyConstant c
  Unary op a      -> prettyOp ++ indentedTerm True 0 (o + length prettyOp) a
                       where prettyOp = prettyUnaryOp op
  Binary op a b   -> if pp then parens binary else binary
                       where binary = prettyTerm a ++ " " ++ prettyBinaryOp op ++ " " ++ prettyTerm b
  Quantify q [] t -> indentedTerm pp 0 o t
  Quantify q vs t -> prettyQ ++ " " ++ prettyVars ++ ": " ++ parens (indentedTerm False 0 o' t)
                       where prettyQ = prettyQuantifier q
                             prettyVars = tuple (map prettyTypedVar vs)
                             o' = o + length prettyQ + 1 + length prettyVars + 3
  Let b t         -> funapp "$let" [offsetBinding (o + 5) b,
                                    "\n" ++ indentedTerm False i' o' t]
                       where (i', o') = if isLet t then (i, o) else (i + 5, o + 5)
  If c a b        -> funapp "$ite" [indentedTerm False 0 (o + 5) c,
                                    "\n" ++ indentedTerm False (i + o + 5) (i + o + 5) a,
                                    "\n" ++ indentedTerm False (i + o + 5) (i + o + 5) b]

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _ = False

prettyTerm :: Term -> String
prettyTerm = indentedTerm False 0 0

thf :: String -> String -> String -> String
thf n it s = funapp "thf" [n, it, s] ++ ".\n"

prettyTypeDeclaration :: (String, Type) -> String
prettyTypeDeclaration (n, t) = thf n "type" (n ++ ": " ++ prettyType t)

prettyConjecture :: Formula -> String
prettyConjecture f = thf "asserts" "conjecture" ("\n" ++ indentedTerm False 4 4 f)

prettyTPTP :: (Signature, Formula) -> String
prettyTPTP (sds, c) = concatMap prettyTypeDeclaration sds ++ prettyConjecture c