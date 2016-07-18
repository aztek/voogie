module Kyckling.FOOL.TPTPretty (
  prettyTPTP
) where

import Data.List
import qualified Kyckling.FOOL.Tuple as Tuple
import Kyckling.FOOL.Tuple (Tuple)
import Data.Char

import Kyckling.Theory
import Kyckling.FOOL

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
    Array t -> funapp "$array" ["$int", prettyType t]
    TupleType ts -> tuple (fmap prettyType ts)
    MaybeType t -> funapp "$maybe" [prettyType t]
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
offsetBinding o (Binding d t) = d' ++ " := " ++ indentedTerm False (0, o + length d' + 4) t
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

indentedTerm :: Bool -> (Int, Int) -> Term -> String
indentedTerm pp (i, o) t = indent i ++ case t of
  IntegerConstant n -> show n
  BooleanConstant b -> if b then "$true" else "$false"

  Variable (Typed v _) -> prettyVar v
  Application f [] -> prettyIdentifier f
  Application f args -> funapp (prettyIdentifier f) (map prettyTerm args)

  Unary op a    -> if isPrefix then prettyOp ++ indentedTerm True (0, o + length prettyOp) a
                               else funapp prettyOp [indentedTerm True (0, o + length prettyOp + 1) a]
                     where (isPrefix, prettyOp) = prettyUnaryOp op
  Binary op a b -> if pp then parens binary else binary
                     where (isInfix, prettyOp) = prettyBinaryOp op
                           binary = if isInfix then infx (prettyTerm a) prettyOp (prettyTerm b)
                                               else funapp prettyOp [prettyTerm a, prettyTerm b]

  Quantify q [] t -> indentedTerm pp (0, o) t
  Quantify q vs t -> prettyQ ++ " " ++ prettyVars ++ ": " ++ parens (indentedTerm False (0, o') t)
                       where prettyQ = prettyQuantifier q
                             prettyVars = "[" ++ list (map prettyVariable vs) ++ "]"
                             o' = o + length prettyQ + 1 + length prettyVars + 3

  Equals s a b -> infx (prettyTerm a) (if s == Pos then "=" else "!=") (prettyTerm b)

  Let b t  -> funapp "$let" [offsetBinding (o + 5) b,
                             newline $ indentedTerm False io t]
                where io = if isLet t then (o, o) else (o + 5, o + 5)
  If c a b -> funapp "$ite" [indentedTerm False (0, 0) c,
                             newline $ indentedTerm False io a,
                             newline $ indentedTerm False io b]
                where io = (o + 5, o + 5)

  Select a i   -> funapp "$select" [prettyTerm a, prettyTerm i]
  Store  a i e -> funapp "$store"  [prettyTerm a, prettyTerm i, prettyTerm e]

  TupleLiteral ts -> tuple (fmap prettyTerm ts)

  Nothing_ _ -> "$nothing"
  Just_ t    -> funapp "$just"     [prettyTerm t]
  IsJust t   -> funapp "$isjust"   [prettyTerm t]
  FromJust t -> funapp "$fromjust" [prettyTerm t]

  Left_  t _  -> funapp "$left"      [prettyTerm t]
  Right_ _ t  -> funapp "$right"     [prettyTerm t]
  IsLeft t    -> funapp "$isleft"    [prettyTerm t]
  FromLeft  t -> funapp "$fromleft"  [prettyTerm t]
  FromRight t -> funapp "$fromright" [prettyTerm t]

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _ = False

prettyTerm :: Term -> String
prettyTerm = indentedTerm False (0, 0)

thf :: String -> String -> String -> String
thf n it s = funapp "thf" [n, it, s] ++ ".\n"

prettyTypeDeclaration :: Typed Name -> String
prettyTypeDeclaration (Typed n t) = thf n "type" (n ++ ": " ++ prettyType t)

prettyConjecture :: Formula -> String
prettyConjecture f = thf "asserts" "conjecture" (newline $ indentedTerm False (4, 4) f)

prettyTPTP :: (Signature, Formula) -> String
prettyTPTP (sds, c) = concatMap prettyTypeDeclaration sds ++ prettyConjecture c