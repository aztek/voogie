module Voogie.FOOL.AST where

import Data.List.NonEmpty (NonEmpty)

import Voogie.AST
import Voogie.Theory

type Identifier = AST Name

type VarList = NonEmpty (Typed (NonEmpty Identifier))

type Term = AST Term'

data Term'
  = IntConst Integer
  | BoolConst Bool
  | Ref Identifier [NonEmpty Term]
  | Unary   UnaryOp  Term
  | Binary  BinaryOp Term Term
  | Ternary          Term Term Term
  | Equals Sign Term Term
  | Quantified Quantifier VarList Term
  deriving (Show, Eq)

type Formula = Term
