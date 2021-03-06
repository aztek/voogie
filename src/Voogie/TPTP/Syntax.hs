{-# LANGUAGE LambdaCase #-}

{-|
Module       : Voogie.TPTP.Syntax
Description  : Concrete syntax of the TPTP language.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.TPTP.Syntax (
  isInfix,
  isPrefix,
  keywords,
  kwdTtf,
  kwdType,
  kwdAxiom,
  kwdConjecture,
  kwdIf,
  kwdLet,
  kwdSelect,
  kwdStore,
  opAssign,
  kwdTypeDecl,
  typeNames,
  intName,
  boolName,
  arrayName
) where

import Voogie.Language

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
  kwdTtf,
  kwdType,
  kwdAxiom,
  kwdConjecture,
  kwdIf,
  kwdLet,
  kwdSelect,
  kwdStore,
  kwdTypeDecl
  ]

kwdTtf, kwdType, kwdAxiom, kwdConjecture :: Name
kwdTtf = "ttf"
kwdType = "type"
kwdAxiom = "axiom"
kwdConjecture = "conjecture"

kwdIf, kwdLet, kwdSelect, kwdStore :: Name
kwdIf = "$ite"
kwdLet = "$let"
kwdSelect = "$select"
kwdStore = "$store"

opAssign :: Name
opAssign = ":="

kwdTypeDecl :: Name
kwdTypeDecl = "$tType"

typeNames :: [Name]
typeNames = [
  intName,
  boolName,
  arrayName
  ]

intName, boolName, arrayName :: Name
intName = "$int"
boolName = "$o"
arrayName = "$array"
