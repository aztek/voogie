module Voogie.TPTPretty (
  prettyTPTP
) where

import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Voogie.FOOL.Tuple as Tuple
import Voogie.FOOL.Tuple (Tuple)
import Data.Char

import Voogie.Theory
import Voogie.FOOL
import Voogie.TPTP

parens :: String -> String
parens s = "(" ++ s ++ ")"

brackets :: String -> String
brackets s = "[" ++ s ++ "]"

list :: [String] -> String
list = intercalate ", "

vars :: NonEmpty String -> String
vars = brackets . list . NE.toList

tuple :: Tuple String -> String
tuple = brackets . list . Tuple.toList

funapp :: String -> [String] -> String
funapp f [] = f
funapp f as = f ++ parens (list as)

prettyBinaryOp :: BinaryOp -> (Bool, String)
prettyBinaryOp op =
  case op of
    And      -> (True,  "&")
    Or       -> (True,  "|")
    Imply    -> (True,  "=>")
    Iff      -> (True,  "<=>")
    Xor      -> (True,  "<~>")
    Greater  -> (False, "$greater")
    Less     -> (False, "$less")
    Geq      -> (False, "$greatereq")
    Leq      -> (False, "$lesseq")
    Add      -> (False, "$sum")
    Subtract -> (False, "$difference")
    Multiply -> (False, "$product")
    Divide   -> (False, "$quotient_f")

prettyUnaryOp :: UnaryOp -> (Bool, String)
prettyUnaryOp op =
  case op of
    Negate   -> (True, "~")
    Positive -> (False, "$uplus")
    Negative -> (False, "$uminus")

prettyType :: Type -> String
prettyType s =
  case s of
    Boolean -> "$o"
    Integer -> "$int"
    Array i t -> foldr (\q w -> funapp "$array" [prettyType q, w]) (prettyType t) i
    TupleType ts -> tuple (fmap prettyType ts)

prettyVar :: Var -> String
prettyVar (Var []) = error "empty variable name"
prettyVar (Var (c:cs)) = toUpper c : cs

prettyIdentifier :: Identifier -> String
prettyIdentifier (Typed [] _) = error "empty identifier"
prettyIdentifier (Typed (c:cs) _) = toLower c : cs

prettyTyped :: Typed String -> String
prettyTyped (Typed s t) = s ++ ":" ++ prettyType t

prettyVariable :: Typed Var -> String
prettyVariable = prettyTyped . fmap prettyVar

prettyDefinition :: Definition -> String
prettyDefinition (Symbol c []) = prettyIdentifier c
prettyDefinition (Symbol c vs) = funapp (prettyIdentifier c) (map prettyVariable vs)
prettyDefinition (TupleD es) = tuple (fmap prettyIdentifier es)

offsetBinding :: Int -> Binding -> String
offsetBinding o (Binding d t) = unwords [d', ":=", offsetTerm (o + length d' + 4) t]
  where d' = prettyDefinition d

prettyBinding :: Binding -> String
prettyBinding = offsetBinding 0

prettyQuantifier :: Quantifier -> String
prettyQuantifier Forall = "!"
prettyQuantifier Exists = "?"

prettyEq :: Sign -> String
prettyEq Pos = "="
prettyEq Neg = "!="

indent :: Int -> String
indent = flip replicate ' '

infx :: String -> String -> String -> String
infx a op b = unwords [a, op, b]

newline :: String -> String
newline = ('\n' :)

indentedTerm :: (Int, Int) -> Term -> String
indentedTerm = indentedTerm' False

indentedTerm' :: Bool -> (Int, Int) -> Term -> String
indentedTerm' pp (i, o) t = newline $ indent i ++ offsetTerm' pp o t

qqq :: String -> Int
qqq s = case lines s of
          [] -> 0
          (h:_) -> length h

offsetTerm :: Int -> Term -> String
offsetTerm = offsetTerm' False

offsetTerm' :: Bool -> Int -> Term -> String
offsetTerm' pp o t = case t of
  IntegerConstant n -> show n
  BooleanConstant b -> if b then "$true" else "$false"

  Variable (Typed v _) -> prettyVar v
  Application f args -> funapp (prettyIdentifier f) (map prettyTerm args)

  Unary op a    -> if isPrefix then prettyOp ++ offsetTerm' True (o + length prettyOp) a
                               else funapp prettyOp [offsetTerm' True (o + length prettyOp + 1) a]
                     where (isPrefix, prettyOp) = prettyUnaryOp op
  Binary op a b -> if isInfix
                   then let o' = o + 3
                            a' = offsetTerm' True o a
                            b' = indentedTerm' True (o', o') b
                         in parens $ infx a' prettyOp b'
                   else funapp prettyOp [prettyTerm a, prettyTerm b]
                     where (isInfix, prettyOp) = prettyBinaryOp op

  Quantify q vs t -> parens $ unwords [prettyQ, prettyVars, ":", parens (offsetTerm o' t)]
                       where prettyQ = prettyQuantifier q
                             prettyVars = vars (fmap prettyVariable vs)
                             o' = o + length prettyQ + 1 + length prettyVars + 5

  Equals s a b -> parens $ infx (prettyTerm a) (prettyEq s) (prettyTerm b)

  Let b t  -> funapp "$let" [offsetBinding (o + 5) b, indentedTerm io t]
                where io = if isLet t then (o, o) else (o + 5, o + 5)

  If c a b -> funapp "$ite" [offsetTerm 0 c, indentedTerm io a, indentedTerm io b]
                where io = (o + 5, o + 5)

  Select a i   -> funapp "$select" [prettyTerm a, prettyTerm i]
  Store  a i e -> funapp "$store"  [prettyTerm a, prettyTerm i, prettyTerm e]

  TupleLiteral ts -> tuple (fmap prettyTerm ts)


isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _ = False

prettyTerm :: Term -> String
prettyTerm = offsetTerm 0

thf :: String -> String -> String -> String
thf n it s = funapp "thf" [n, it, s] ++ ".\n"

prettyTypeUnit :: Typed Name -> String
prettyTypeUnit n@(Typed s t) = thf s "type" (parens $ prettyTyped n)

prettyAxiom :: Formula -> Integer -> String
prettyAxiom f nr = thf ("voogie_precondition_" ++ show nr) "axiom" (indentedTerm (4, 4) f)

prettyConjecture :: Formula -> String
prettyConjecture f = thf "voogie_conjecture" "conjecture" (indentedTerm (4, 4) f)

prettyUnit :: (String, Integer) -> Unit -> (String, Integer)
prettyUnit (s, nr) (Type t)       = (s ++ prettyTypeUnit t,   nr)
prettyUnit (s, nr) (Axiom f)      = (s ++ prettyAxiom f nr,   nr + 1)
prettyUnit (s, nr) (Conjecture f) = (s ++ prettyConjecture f, nr)

prettyTPTP :: TPTP -> String
prettyTPTP (TPTP units) = fst $ foldl prettyUnit ("", 0) units