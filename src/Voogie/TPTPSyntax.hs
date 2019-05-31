{-# LANGUAGE LambdaCase #-}

module Voogie.TPTPSyntax where

import Voogie.Theory

instance Named BinaryOp where
  nameOf = \case
    And      -> "&"
    Or       -> "|"
    Imply    -> "=>"
    Iff      -> "<=>"
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
  Greater  -> False
  Less     -> False
  Geq      -> False
  Leq      -> False
  Add      -> False
  Subtract -> False
  Multiply -> False
  Divide   -> False

instance Named UnaryOp where
  nameOf = \case
    Negate   -> "~"
    Negative -> "$uminus"

isPrefix :: UnaryOp -> Bool
isPrefix = \case
  Negate   -> True
  Negative -> False

instance Named Quantifier where
  nameOf = \case
    Forall -> "!"
    Exists -> "?"

instance Named Sign where
  nameOf = \case
    Pos -> "="
    Neg -> "!="

instance Named Bool where
  nameOf = \case
    True  -> "$true"
    False -> "$false"

keywords :: [Name]
keywords = [
    kwdTtf
  , kwdType
  , kwdAxiom
  , kwdConjecture
  , kwdIf
  , kwdLet
  , kwdSelect
  , kwdStore
  , kwdTypeDecl
  ]

kwdTtf = "ttf"
kwdType = "type"
kwdAxiom = "axiom"
kwdConjecture = "conjecture"

kwdIf = "$ite"
kwdLet = "$let"
kwdSelect = "$select"
kwdStore = "$store"

opAssign :: Name
opAssign = ":="

kwdTypeDecl = "$tType"

typeNames :: [Name]
typeNames = [
    intName
  , boolName
  , arrayName
  ]

intName = "$int"
boolName = "$o"
arrayName = "$array"
