module Voogie.TPTPretty (prettyTPTP) where

import Data.List.NonEmpty (NonEmpty)
import qualified Voogie.NonEmpty as VNE
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

list :: NonEmpty String -> String
list = VNE.intercalate ", "

vars :: NonEmpty String -> String
vars = brackets . list

tuple :: Tuple String -> String
tuple = brackets . list . Tuple.toNonEmpty

funapp :: String -> NonEmpty String -> String
funapp f as = f ++ parens (list as)

funapp1 :: String -> String -> String
funapp1 f a = funapp f (VNE.one a)

funapp2 :: String -> String -> String -> String
funapp2 f a b = funapp f (VNE.two a b)

funapp3 :: String -> String -> String -> String -> String
funapp3 f a b c = funapp f (VNE.three a b c)

prettyBinaryOp :: BinaryOp -> (Bool, String)
prettyBinaryOp op = case op of
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
  Divide   -> (False, "$quotient_e")

prettyUnaryOp :: UnaryOp -> (Bool, String)
prettyUnaryOp op = case op of
  Negate   -> (True, "~")
  Positive -> (False, "$uplus")
  Negative -> (False, "$uminus")

prettyType :: Type -> String
prettyType s = case s of
  Boolean -> "$o"
  Integer -> "$int"
  Array i t -> foldr (funapp2 "$array") (prettyType t) (fmap prettyType i)
  TupleType ts -> tuple (fmap prettyType ts)
    where ts' = VNE.intercalate " * " (fmap prettyType ts)
  Custom n -> n

prettyVar :: Var -> String
prettyVar (Var []) = error "empty variable name"
prettyVar (Var (c:cs)) = toUpper c : cs

prettyIdentifier :: Identifier -> String
prettyIdentifier (Typed _ []) = error "empty identifier"
prettyIdentifier (Typed _ (c:cs)) = toLower c : cs

prettyTyped :: Identifier -> String
prettyTyped (Typed t s) = unwords [s, ":", prettyType t]

prettyVariable :: Typed Var -> String
prettyVariable = prettyTyped . fmap prettyVar

prettyDefinition :: Definition -> String
prettyDefinition (ConstantSymbol c) = prettyIdentifier c
prettyDefinition (Function c vs) = funapp (prettyIdentifier c)
                                          (fmap prettyVariable vs)
prettyDefinition (TupleD es) = tuple (fmap prettyIdentifier es)

prettyTypedDefinition :: Definition -> String
prettyTypedDefinition (ConstantSymbol c) = prettyTyped c
prettyTypedDefinition (Function c _) = prettyTyped c
prettyTypedDefinition (TupleD es) = tuple (prettyTyped <$> es)

indentedBinding :: (Int, Int) -> Binding -> String
indentedBinding (i, o) (Binding d t) =
  newline $ indent i ++ unwords [d', ":=", offsetTerm o' t]
  where
    d' = prettyDefinition d
    o' = o + length d' + 4

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
indentedTerm (i, o) t = newline $ indent i ++ offsetTerm o t

offsetTerm :: Int -> Term -> String
offsetTerm o t = case t of
  IntegerConstant n -> show n
  BooleanConstant b -> if b then "$true" else "$false"

  Variable (Typed _ v) -> prettyVar v
  Constant f -> prettyIdentifier f
  Application f args -> funapp (prettyIdentifier f) (fmap prettyTerm args)

  Unary op a | isPrefix  -> prettyOp ++ offsetTerm o' a
             | otherwise -> funapp1 prettyOp (offsetTerm (o' + 1) a)
    where (isPrefix, prettyOp) = prettyUnaryOp op
          o' = o + length prettyOp

  Binary op a b | isInfix  -> let o' = o + 3
                                  a' = offsetTerm o a
                                  b' = indentedTerm (o', o') b
                               in parens $ infx a' prettyOp b'
                | otherwise -> funapp2 prettyOp (prettyTerm a) (prettyTerm b)
    where (isInfix, prettyOp) = prettyBinaryOp op

  Quantify q vs t -> parens $ unwords [ prettyQ
                                      , prettyVars
                                      , ":"
                                      , parens (offsetTerm o' t)
                                      ]
    where prettyQ = prettyQuantifier q
          prettyVars = vars (fmap prettyVariable vs)
          o' = o + length prettyQ + 1 + length prettyVars + 5

  Equals s a b -> parens $ infx (prettyTerm a) (prettyEq s) (prettyTerm b)

  Let b@(Binding d _) t -> funapp3 "$let" d'
                                          (indentedBinding (i', o') b)
                                          (indentedTerm io t)
    where d' = prettyTypedDefinition d
          i' = o + 5
          o' = o + 5
          io = if isLet t then (o, o) else (o', o')

  If c a b -> funapp3 "$ite" (prettyTerm c) (indentedTerm io a) (indentedTerm io b)
    where io = (o', o')
          o' = o + 5

  Select a i   -> funapp2 "$select" (prettyTerm a) (prettyTerm i)
  Store  a i e -> funapp3 "$store"  (prettyTerm a) (prettyTerm i) (prettyTerm e)

  TupleLiteral ts -> tuple (fmap prettyTerm ts)


isLet :: Term -> Bool
isLet Let{} = True
isLet _ = False

prettyTerm :: Term -> String
prettyTerm = offsetTerm 0

tff :: String -> String -> String -> String
tff n it s = funapp3 "tff" n it s ++ ".\n"

prettyTypeDeclaration :: Name -> String
prettyTypeDeclaration n = tff n "type" (n ++ " : $tType")

prettySymbolDeclaration :: Typed Name -> String
prettySymbolDeclaration n@(Typed _ s) = tff s "type"

prettyAxiom :: (Integer, Formula) -> String
prettyAxiom (nr, f) = tff ("voogie_precondition_" ++ show nr) "axiom"
                          (indentedTerm (4, 4) f)

prettyConjecture :: Formula -> String
prettyConjecture f = tff "voogie_conjecture" "conjecture"
                         (indentedTerm (4, 4) f)

prettyTPTP :: TPTP -> String
prettyTPTP (TPTP types symbols axioms conjecture) =
  concatMap prettyTypeDeclaration types ++
  concatMap prettySymbolDeclaration symbols ++
  concatMap prettyAxiom (zip [1..] axioms) ++
  prettyConjecture conjecture
