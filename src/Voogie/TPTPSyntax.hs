module Voogie.TPTPSyntax where

import Voogie.Theory

binaryOpName :: BinaryOp -> String
binaryOpName = \case
  And      -> "&"
  Or       -> "|"
  Imply    -> "=>"
  Iff      -> "<=>"
  Xor      -> "<~>"
  Greater  -> "$greater"
  Less     -> "$less"
  Geq      -> "$greatereq"
  Leq      -> "$lesseq"
  Add      -> "$sum"
  Subtract -> "$difference"
  Multiply -> "$product"
  Divide   -> "$quotient_e"

isInfix :: BinaryOp -> Bool
isInfix = \case
  And      -> True
  Or       -> True
  Imply    -> True
  Iff      -> True
  Xor      -> True
  Greater  -> False
  Less     -> False
  Geq      -> False
  Leq      -> False
  Add      -> False
  Subtract -> False
  Multiply -> False
  Divide   -> False

unaryOpName :: UnaryOp -> String
unaryOpName = \case
  Negate   -> "~"
  Negative -> "$uminus"

isPrefix :: UnaryOp -> Bool
isPrefix = \case
  Negate   -> True
  Negative -> False

quantifierName :: Quantifier -> String
quantifierName = \case
  Forall -> "!"
  Exists -> "?"

signName :: Sign -> String
signName = \case
  Pos -> "="
  Neg -> "!="

booleanName :: Bool -> String
booleanName True = "$true"
booleanName False = "$false"

kwdTtf = "ttf"
kwdType = "type"
kwdAxiom = "axiom"
kwdConjecture = "conjecture"

kwdIf = "$ite"
kwdLet = "$let"
kwdSelect = "$select"
kwdStore = "$store"

opAssign = ":="

kwdTypeDecl = "$tType"
intName = "$int"
boolName = "$o"
arrayName = "$array"
