{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Voogie.FOOL (
  Var(..), VarList, Identifier,
  Definition(..), Binding(..), Term(..),
  Formula, Conjunction(..),
  Theory(..), Problem(..), appendTheory, appendTheories
) where

import Data.List.NonEmpty (NonEmpty)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup(..))
#endif

import Voogie.FOOL.Tuple (Tuple)
import Voogie.Theory

newtype Var = Var Name
  deriving (Show, Eq, Ord)

instance Named Var where
  nameOf (Var n) = n

type Identifier = Typed Name

data Definition
  = ConstantSymbol Identifier
  | Function Identifier (NonEmpty (Typed Var))
  | TupleD (Tuple Identifier)
  deriving (Show, Eq, Ord)

data Binding = Binding Definition Term
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

type Formula = Term

instance TypeOf Term where
  typeOf = \case
    IntegerConstant _ -> Integer
    BooleanConstant _ -> Boolean
    Variable        v -> typeOf v
    Constant        i -> typeOf i
    Application   i _ -> returnType (typeOf i)
    Binary     op _ _ -> binaryOpRange op
    Unary        op _ -> unaryOpRange op
    Quantify{}        -> Boolean
    Equals{}          -> Boolean
    Let           _ t -> typeOf t
    If          _ a _ -> typeOf a
    Select        a _ -> arrayArgument (typeOf a)
    Store       a _ _ -> typeOf a
    TupleLiteral   es -> TupleType (fmap typeOf es)

newtype Conjunction = Conjunction { getConjunction :: Formula }
  deriving (Eq, Show)

instance Semigroup Conjunction where
  Conjunction x <> Conjunction y = Conjunction (binaryAnd x y)
    where
      binaryAnd :: Formula -> Formula -> Formula
      binaryAnd f (BooleanConstant True) = f
      binaryAnd (BooleanConstant True) g = g
      -- This case is only needed to satisfy the associativity law of Semigroup
      binaryAnd (Binary And f g) h = Binary And f (Binary And g h)
      binaryAnd f g = Binary And f g

instance Monoid Conjunction where
  mempty = Conjunction (BooleanConstant True)
  mappend = (<>)

data Theory = Theory [Name] [Identifier] [Formula]
  deriving (Show, Eq, Ord)

instance Semigroup Theory where
  Theory ts ss as <> Theory ts' ss' as' =
    Theory (ts <> ts') (ss <> ss') (as <> as')

instance Monoid Theory where
  mempty = Theory mempty mempty mempty
  mappend = (<>)

data Problem = Problem {
  types      :: [Name],
  symbols    :: [Identifier],
  axioms     :: [Formula],
  conjecture :: Formula
} deriving (Show, Eq, Ord)

appendTheory :: Problem -> Theory -> Problem
appendTheory (Problem ts ss as c) th = Problem ts' ss' as' c
  where Theory ts' ss' as' = th <> Theory ts ss as

appendTheories :: Problem -> [Theory] -> Problem
appendTheories p = appendTheory p . mconcat
