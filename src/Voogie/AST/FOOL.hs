module Voogie.AST.FOOL where

import Data.List.NonEmpty (NonEmpty)

import Voogie.AST
import Voogie.Theory

type Identifier = AST Name

type VarList = NonEmpty (Typed (NonEmpty Identifier))

type Term = AST TermF
data TermF
  = IntegerConstant Integer
  | BooleanConstant Bool
  | Ref Identifier [NonEmpty Term]
  | Unary UnaryOp Term
  | Binary BinaryOp Term Term
  | IfElse Term Term Term
  | Equals Sign Term Term
  | Quantify Quantifier VarList Term
  deriving (Show, Eq)

type Formula = Term
