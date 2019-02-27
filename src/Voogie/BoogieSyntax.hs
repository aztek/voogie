module Voogie.BoogieSyntax where

import Voogie.Theory

unaryOpName :: UnaryOp -> Name
unaryOpName = \case
  Negate   -> "!"
  Negative -> "-"

binaryOpName :: BinaryOp -> Name
binaryOpName = \case
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
comparePrecedence op1 op2 = compare (precedence op1) (precedence op2)

comparePrecedenceEquality :: BinaryOp -> Ordering
comparePrecedenceEquality op = compare 7 (precedence op)

signName :: Sign -> Name
signName = \case
  Pos -> "=="
  Neg -> "!="

quantifierName :: Quantifier -> Name
quantifierName = \case
  Forall -> "forall"
  Exists -> "exists"

booleanName :: Bool -> Name
booleanName = \case
  True  -> "true"
  False -> "false"

keywords :: [Name]
keywords = [
    kwdAssert
  , kwdAssume
  , kwdVar
  , kwdIf
  , kwdThen
  , kwdElse
  , kwdProcedure
  , kwdMain
  , kwdRequires
  , kwdEnsures
  , kwdReturns
  , kwdModifies
  ]

kwdAssert = "assert"
kwdAssume = "assume"
kwdVar = "var"
kwdIf = "if"
kwdThen = "then"
kwdElse = "else"

kwdProcedure = "procedure"
kwdMain = "main"
kwdRequires = "requires"
kwdEnsures = "ensures"
kwdReturns = "returns"
kwdModifies = "modifies"

operatorNames :: [Name]
operatorNames = [
    opAssign
  , opTyped
  ]

opAssign = ":="
opTyped = ":"

typeNames :: [Name]
typeNames = [
    typeInteger
  , typeBoolean
  ]

typeInteger = "int"
typeBoolean = "bool"
