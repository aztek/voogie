module Voogie.FOOL where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Voogie.FOOL.Tuple (Tuple)

import Voogie.Theory

newtype Var = Var Name
  deriving (Show, Eq)

type Identifier = Typed Name

data Definition = ConstantSymbol Identifier
                | Function Identifier (NonEmpty (Typed Var))
                | TupleD (Tuple Identifier)
  deriving (Show, Eq)

data Binding = Binding Definition Term
  deriving (Show, Eq)

data Term = IntegerConstant Integer
          | BooleanConstant Bool
          | Variable (Typed Var)
          | Constant Identifier
          | Application Identifier (NonEmpty Term)
          | Binary BinaryOp Term Term
          | Unary UnaryOp Term
          | Quantify Quantifier (NonEmpty (Typed Var)) Term
          | Equals Sign Term Term
          | Let Binding Term
          | If Term Term Term
          -- Arrays
          | Select Term Term
          | Store Term Term Term
          -- Tuples
          | TupleLiteral (Tuple Term)
  deriving (Show, Eq)

type Formula = Term

instance TypeOf Term where
  typeOf (IntegerConstant _) = Integer
  typeOf (BooleanConstant _) = Boolean
  typeOf (Variable v) = typeOf v
  typeOf (Constant i) = typeOf i
  typeOf (Application i _) = typeOf i
  typeOf (Binary op _ _) = binaryOpRange op
  typeOf (Unary  op _) = unaryOpRange op
  typeOf Quantify{} = Boolean
  typeOf Equals{} = Boolean
  typeOf (Let _ t) = typeOf t
  typeOf (If _ a _) = typeOf a
  typeOf (Select array indexes) = arrayArgument (typeOf array)
  typeOf (Store array _ _) = typeOf array
  typeOf (TupleLiteral args) = TupleType (fmap typeOf args)