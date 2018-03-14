module Voogie.FOOL.AST where

import Data.List.NonEmpty (NonEmpty)

import Voogie.AST
import Voogie.Theory

type VarList = NonEmpty (Typed (NonEmpty String))

type Term = AST Term'

data Term'
  = IntConst Integer
  | BoolConst Bool
  | Ref String [NonEmpty Term]
  | Unary   UnaryOp  Term
  | Binary  BinaryOp Term Term
  | Ternary          Term Term Term
  | Equals Sign Term Term
  | Quantified Quantifier VarList Term
  deriving (Show, Eq)

type Formula = Term
