module Kyckling.FOOL.AST where

import Kyckling.Theory

data Term = IntConst Integer
          | BoolConst Bool
          | Unary   UnaryOp  Term
          | Binary  BinaryOp Term Term
          | Ternary          Term Term Term
          | Eql   Term Term
          | InEql Term Term
          | Quantified Quantifier [Typed Name] Term
          | Constant Name
          | ArrayElem Name Term
  deriving (Show, Eq)

type Formula = Term