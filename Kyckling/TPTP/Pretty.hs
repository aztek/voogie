module Kyckling.TPTP.Pretty (
  prettyTPTP
) where

import Data.List

import Kyckling.TPTP

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

prettySort :: Sort -> String
prettySort s =
  case s of
    Boolean    -> "$o"
    Integer    -> "$int"
    TupleS ss  -> tuple (map prettySort ss)
    Array i t  -> funapp "$array" [prettySort i, prettySort t]

prettySortedVar :: SortedVar -> String
prettySortedVar (SortedVar v s) = v ++ ":" ++ prettySort s

prettyDefinition :: Definition -> String
prettyDefinition (Symbol c []) = c
prettyDefinition (Symbol c vs) = funapp c (map prettySortedVar vs)
prettyDefinition (TupleD es) = tuple es

offsetBinding :: Int -> Binding -> String
offsetBinding o (Binding d t) = d' ++ " := " ++ indentedTerm 0 (o + length d' + 4) t
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

indentedTerm :: Int -> Int -> Term -> String
indentedTerm i o t = indent i ++ case t of
  IntegerConst num   -> show num
  BooleanConst True  -> "$true"
  BooleanConst False -> "$false"
  FunApp Tuple ts    -> tuple (map prettyTerm ts)
  FunApp f []        -> prettyFun f
  FunApp f ts        -> funapp (prettyFun f) (map prettyTerm ts)
  Var v              -> v
  Constant c         -> c
  Unary op a         -> prettyUnaryOp op ++ prettyTerm a
  Binary op a b      -> parens $ prettyTerm a ++ " " ++ prettyBinaryOp op ++ " " ++ prettyTerm b
  Quantify q [] t    -> prettyTerm t
  Quantify q vs t    -> prettyQuantifier q ++ " " ++ tuple (map prettySortedVar vs) ++ ": " ++ prettyTerm t
  Let b t            -> funapp "$let" [offsetBinding (o + 5) b,
                               "\n" ++ indentedTerm (if isLet t then i else i + 5) (if isLet t then o else o + 5) t]
  If c a b           -> funapp "$ite" [indentedTerm 0 (o + 5) c,
                               "\n" ++ indentedTerm (i + o + 5) (i + o + 5) a,
                               "\n" ++ indentedTerm (i + o + 5) (i + o + 5) b]

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _ = False

prettyTerm :: Term -> String
prettyTerm = indentedTerm 0 0

thf :: String -> String -> String -> String
thf n it s = funapp "thf" [n , it, "\n" ++ s] ++ ".\n"

prettySortDeclaration :: SortDeclaration -> String
prettySortDeclaration (SortDeclaration c s) = thf c "type" (c ++ ": " ++ prettySort s)

prettyConjecture :: Conjecture -> String
prettyConjecture (Conjecture n t) = thf n "conjecture" (indentedTerm 4 4 t)
  where i = 4 + length n

prettyTPTP :: TPTP -> String
prettyTPTP (TPTP sds c) = unwords (map prettySortDeclaration sds) ++ prettyConjecture c