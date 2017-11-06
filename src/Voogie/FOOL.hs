module Voogie.FOOL where

import Data.List.NonEmpty (NonEmpty)

import Voogie.FOOL.Tuple (Tuple)
import Voogie.Theory

newtype Var = Var Name
  deriving (Show, Eq)

type Identifier = Typed Name

data Definition
  = ConstantSymbol Identifier
  | Function Identifier (NonEmpty (Typed Var))
  | TupleD (Tuple Identifier)
  deriving (Show, Eq)

data Binding = Binding Definition Term
  deriving (Show, Eq)

type VarList = NonEmpty (Typed Var)

data Term
  = IntegerConstant Integer
  | BooleanConstant Bool
  | Variable (Typed Var)
  | Constant Identifier
  | Application Identifier (NonEmpty Term)
  | Binary BinaryOp Term Term
  | Unary UnaryOp Term
  | Quantify Quantifier VarList Term
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
  typeOf (Select array _) = arrayArgument (typeOf array)
  typeOf (Store array _ _) = typeOf array
  typeOf (TupleLiteral args) = TupleType (fmap typeOf args)

newtype Conjunction = Conjunction { getConjunction :: Formula }
  deriving (Eq, Show)

instance Monoid Conjunction where
  mempty = Conjunction (BooleanConstant True)
  Conjunction f `mappend` Conjunction g = Conjunction (binaryAnd f g)
    where
      binaryAnd :: Formula -> Formula -> Formula
      binaryAnd f (BooleanConstant True) = f
      binaryAnd (BooleanConstant True) g = g
      -- This case is only needed to satisfy the associativity law of Monoid
      binaryAnd (Binary And f g) h = Binary And f (Binary And g h)
      binaryAnd f g = Binary And f g
