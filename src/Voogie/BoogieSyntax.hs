module Voogie.BoogieSyntax where

import Voogie.Theory

unaryOpName :: UnaryOp -> String
unaryOpName = \case
  Negate   -> "!"
  Positive -> "+"
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

isAssociative :: BinaryOp -> Bool
isAssociative = \case
  And      -> True
  Or       -> True
  Imply    -> False
  Iff      -> False
  Xor      -> False
  Greater  -> False
  Less     -> False
  Geq      -> False
  Leq      -> False
  Add      -> True
  Subtract -> False
  Multiply -> True
  Divide   -> False

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

kwdAssume = "assume"
kwdVar = "var"
kwdIf = "if"
kwdElse = "else"
opAssign = ":="

kwdProcedure = "procedure"
kwdMain = "main"
kwdRequires = "requires"
kwdEnsures = "ensures"
kwdReturns = "returns"
kwdModifies = "modifies"