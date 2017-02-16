module Voogie.FOOL.AST where

import Data.List.NonEmpty (NonEmpty)

import Voogie.Theory

data Term = IntConst Integer
          | BoolConst Bool
          | Const String
          | Unary   UnaryOp  Term
          | Binary  BinaryOp Term Term
          | Ternary          Term Term Term
          | Equals Sign Term Term
          | Quantified Quantifier (NonEmpty (Typed (NonEmpty String))) Term
          | ArrayElem String (NonEmpty Term)
  deriving (Show, Eq)

type Formula = Term
