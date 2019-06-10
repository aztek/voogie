{-# LANGUAGE LambdaCase #-}

module Voogie.Boogie.Syntax (
  precedence,
  comparePrecedence,
  comparePrecedenceEquality,
  keywords,
  kwdAssert,
  kwdAssume,
  kwdVar,
  kwdIf,
  kwdThen,
  kwdElse,
  kwdProcedure,
  kwdMain,
  kwdRequires,
  kwdEnsures,
  kwdReturns,
  kwdModifies,
  operatorNames,
  opAssign,
  opTyped,
  opQsep,
  typeNames,
  typeInteger,
  typeBoolean
) where

import Data.Function (on)

import Voogie.Theory

instance Named UnaryOp where
  nameOf = \case
    Negate   -> "!"
    Negative -> "-"

instance Named BinaryOp where
  nameOf = \case
    And      -> "&&"
    Or       -> "||"
    Imply    -> "==>"
    Iff      -> "<==>"
    Greater  -> ">"
    Less     -> "<"
    Geq      -> ">="
    Leq      -> "<="
    Add      -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide   -> "div"

precedence :: BinaryOp -> Int
precedence = \case
  Multiply -> 10
  Divide   -> 10
  Add      -> 9
  Subtract -> 9
  Greater  -> 8
  Less     -> 8
  Geq      -> 8
  Leq      -> 8
  And      -> 6
  Or       -> 6
  Imply    -> 5
  Iff      -> 4

comparePrecedence :: BinaryOp -> BinaryOp -> Ordering
comparePrecedence = compare `on` precedence

comparePrecedenceEquality :: BinaryOp -> Ordering
comparePrecedenceEquality op = 7 `compare` precedence op

instance Named Sign where
  nameOf = \case
    Pos -> "=="
    Neg -> "!="

instance Named Quantifier where
  nameOf = \case
    Forall -> "forall"
    Exists -> "exists"

instance Named Bool where
  nameOf = \case
    True  -> "true"
    False -> "false"

keywords :: [Name]
keywords = [
  kwdAssert,
  kwdAssume,
  kwdVar,
  kwdIf,
  kwdThen,
  kwdElse,
  kwdProcedure,
  kwdMain,
  kwdRequires,
  kwdEnsures,
  kwdReturns,
  kwdModifies
  ]

kwdAssert, kwdAssume, kwdVar, kwdIf, kwdThen, kwdElse :: Name
kwdAssert = "assert"
kwdAssume = "assume"
kwdVar    = "var"
kwdIf     = "if"
kwdThen   = "then"
kwdElse   = "else"

kwdProcedure, kwdMain, kwdRequires, kwdEnsures, kwdReturns, kwdModifies :: Name
kwdProcedure = "procedure"
kwdMain      = "main"
kwdRequires  = "requires"
kwdEnsures   = "ensures"
kwdReturns   = "returns"
kwdModifies  = "modifies"

operatorNames :: [Name]
operatorNames = [
  opAssign,
  opTyped,
  opQsep
  ]

opAssign, opTyped, opQsep :: Name
opAssign = ":="
opTyped = ":"
opQsep = "::"

typeNames :: [Name]
typeNames = [
  typeInteger,
  typeBoolean
  ]

typeInteger, typeBoolean :: Name
typeInteger = "int"
typeBoolean = "bool"
