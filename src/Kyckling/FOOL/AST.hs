module Kyckling.FOOL.AST where

import Kyckling.Theory

data Term = IntConst Integer
          | BoolConst Bool
          | Const String
          | Unary   UnaryOp  Term
          | Binary  BinaryOp Term Term
          | Ternary          Term Term Term
          | Equals Sign Term Term
          | Quantified Quantifier [Typed String] Term
          | FunApp String [Term]
          | ArrayElem String Term
  deriving (Show, Eq)

type Formula = Term