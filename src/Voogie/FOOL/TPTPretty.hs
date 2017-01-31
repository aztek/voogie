module Voogie.FOOL.TPTPretty (
  prettyTPTP
) where

import Data.List
import qualified Voogie.FOOL.Tuple as Tuple
import Voogie.FOOL.Tuple (Tuple)
import Data.Char

import Voogie.Theory
import Voogie.FOOL

list :: [String] -> String
list = intercalate ", "

tuple :: Tuple String -> String
tuple es = "[" ++ Tuple.intercalate ", " es ++ "]"

parens :: String -> String
parens s = "(" ++ s ++ ")"

funapp :: String -> [String] -> String
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
    Array i t -> funapp "$array" [prettyType i, prettyType t]
    TupleType ts -> tuple (fmap prettyType ts)
    OptionType t -> funapp "$option" [prettyType t]
    EitherType l r -> funapp "$either" [prettyType l, prettyType r]

prettyVar :: Var -> String
prettyVar (Var n) = n

prettyIdentifier :: Identifier -> String
prettyIdentifier (Typed n _) = n

prettyVariable :: Typed Var -> String
prettyVariable (Typed v t) = prettyVar v ++ ":" ++ prettyType t

prettyDefinition :: Definition -> String
prettyDefinition (Symbol c []) = prettyIdentifier c
prettyDefinition (Symbol c vs) = funapp (prettyIdentifier c) (map prettyVariable vs)
prettyDefinition (TupleD es) = tuple (fmap prettyIdentifier es)

offsetBinding :: Int -> Binding -> String
offsetBinding o (Binding d t) = d' ++ " := " ++ offsetTerm (o + length d' + 4) t
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

infx :: String -> String -> String -> String
infx a op b = a ++ " " ++ op ++ " " ++ b

newline :: String -> String
newline = ('\n' :)

indentedTerm :: (Int, Int) -> Term -> String
indentedTerm = indentedTerm' False

indentedTerm' :: Bool -> (Int, Int) -> Term -> String
indentedTerm' pp (i, o) t = newline $ indent i ++ offsetTerm' pp o t

offsetTerm :: Int -> Term -> String
offsetTerm = offsetTerm' False

offsetTerm' :: Bool -> Int -> Term -> String
offsetTerm' pp o t = case t of
  IntegerConstant n -> show n
  BooleanConstant b -> if b then "$true" else "$false"

  Variable (Typed v _) -> prettyVar v
  Application f [] -> prettyIdentifier f
  Application f args -> funapp (prettyIdentifier f) (map prettyTerm args)

  Unary op a    -> if isPrefix then prettyOp ++ offsetTerm' True (o + length prettyOp) a
                               else funapp prettyOp [offsetTerm' True (o + length prettyOp + 1) a]
                     where (isPrefix, prettyOp) = prettyUnaryOp op
  Binary op a b -> if pp then parens binary else binary
                     where (isInfix, prettyOp) = prettyBinaryOp op
                           binary = if isInfix then infx (prettyTerm a) prettyOp (prettyTerm b)
                                               else funapp prettyOp [prettyTerm a, prettyTerm b]

  Quantify q [] t -> offsetTerm' pp o t
  Quantify q vs t -> prettyQ ++ " " ++ prettyVars ++ ": " ++ parens (offsetTerm o' t)
                       where prettyQ = prettyQuantifier q
                             prettyVars = "[" ++ list (map prettyVariable vs) ++ "]"
                             o' = o + length prettyQ + 1 + length prettyVars + 3

  Equals s a b -> infx (prettyTerm a) (if s == Pos then "=" else "!=") (prettyTerm b)

  Let b t  -> funapp "$let" [offsetBinding (o + 5) b,
                             indentedTerm io t]
                where io = if isLet t then (o, o) else (o + 5, o + 5)
  If c a b -> funapp "$ite" [offsetTerm 0 c,
                             indentedTerm io a,
                             indentedTerm io b]
                where io = (o + 5, o + 5)

  Select a i   -> funapp "$select" [prettyTerm a, prettyTerm i]
  Store  a i e -> funapp "$store"  [prettyTerm a, prettyTerm i, prettyTerm e]

  TupleLiteral ts -> tuple (fmap prettyTerm ts)

  None t     -> funapp "$none"     [prettyType t]
  Some a     -> funapp "$some"     [offsetTerm (o + 6) a]
  IsSome a   -> funapp "$issome"   [offsetTerm (o + 8)  a]
  FromSome a -> funapp "$fromsome" [offsetTerm (o + 10) a]

  Left_  l t  -> funapp "$left"      [offsetTerm (o + 6) l, prettyType t]
  Right_ t r  -> funapp "$right"     [prettyType t, prettyTerm r]
  IsLeft t    -> funapp "$isleft"    [offsetTerm (o + 7)  t]
  FromLeft  t -> funapp "$fromleft"  [offsetTerm (o + 10) t]
  FromRight t -> funapp "$fromright" [offsetTerm (o + 11) t]

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _ = False

prettyTerm :: Term -> String
prettyTerm = offsetTerm 0

thf :: String -> String -> String -> String
thf n it s = funapp "thf" [n, it, s] ++ ".\n"

prettyTypeDeclaration :: Typed Name -> String
prettyTypeDeclaration (Typed n t) = thf n "type" (n ++ ": " ++ prettyType t)

prettyConjecture :: Formula -> String
prettyConjecture f = thf "asserts" "conjecture" (indentedTerm (4, 4) f)

prettyTPTP :: (Signature, Formula) -> String
prettyTPTP (sds, c) = concatMap prettyTypeDeclaration sds ++ prettyConjecture c
