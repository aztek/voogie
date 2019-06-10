{-|
Module       : Voogie.AST.FOOL
Description  : Abstract syntax tree of a FOOL formula.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.AST.FOOL (
  Identifier,
  VarList,
  Term,
  TermF(..),
  Formula
) where

import Data.List.NonEmpty (NonEmpty)

import Voogie.AST
import Voogie.Language

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
