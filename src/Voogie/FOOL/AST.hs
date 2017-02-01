module Voogie.FOOL.AST where

import Voogie.Theory

data Term = IntConst Integer
          | BoolConst Bool
          | Const String
          | Unary   UnaryOp  Term
          | Binary  BinaryOp Term Term
          | Ternary          Term Term Term
          | Equals Sign Term Term
          | Quantified Quantifier [Typed String] Term
          | ArrayElem String Term
  deriving (Show, Eq)

type Formula = Term
