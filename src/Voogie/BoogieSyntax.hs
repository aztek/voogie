module Voogie.BoogieSyntax where

import Voogie.Theory

unaryOpName :: UnaryOp -> String
unaryOpName = \case
  Negate   -> "!"
  Negative -> "-"

binaryOpName :: BinaryOp -> String
binaryOpName = \case
  And      -> "&&"
  Or       -> "||"
  Imply    -> "==>"
  Iff      -> "<==>"
  Xor      -> "!="
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
  Iff      -> 5
  Xor      -> 5
  Imply    -> 4

comparePrecedence :: BinaryOp -> BinaryOp -> Ordering
comparePrecedence op1 op2 = compare (precedence op1) (precedence op2)

comparePrecedenceEquality :: BinaryOp -> Ordering
comparePrecedenceEquality op = compare 7 (precedence op)

signName :: Sign -> String
signName = \case
  Pos -> "=="
  Neg -> "!="

quantifierName :: Quantifier -> String
quantifierName = \case
  Forall -> "forall"
  Exists -> "exists"

booleanName :: Bool -> String
booleanName = \case
  True  -> "true"
  False -> "false"

kwdAssert = "assert"
kwdAssume = "assume"
kwdVar = "var"
kwdIf = "if"
kwdElse = "else"

kwdProcedure = "procedure"
kwdMain = "main"
kwdRequires = "requires"
kwdEnsures = "ensures"
kwdReturns = "returns"
kwdModifies = "modifies"

keywords = [
    kwdAssert
  , kwdAssume
  , kwdVar
  , kwdIf
  , kwdElse
  , kwdProcedure
  , kwdMain
  , kwdRequires
  , kwdEnsures
  , kwdReturns
  , kwdModifies
  ]

opAssign = ":="
opTyped = ":"

operatorNames = [
    opAssign
  , opTyped
  ]

typeInteger = "int"
typeBoolean = "bool"
