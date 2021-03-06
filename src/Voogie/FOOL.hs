{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module       : Voogie.FOOL
Description  : The FOOL language.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.FOOL (
  module Voogie.Language,
  Var(..),
  VarList,
  Identifier,
  Definition(..),
  Binding(..),
  Term(..),
  foldStore,
  Formula,
  Conjunction(..),
  Theory(..),
  Problem(..),
  appendTheory,
  appendTheories
) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE (nonEmpty)
import Data.List.NonUnit (NonUnit)

#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup(..))
#endif

import Voogie.Language

newtype Var = Var Name
  deriving (Show, Eq, Ord)

instance Named Var where
  nameOf (Var n) = n

type Identifier = Typed Name

data Definition
  = ConstantSymbol Identifier
  | Function Identifier (NonEmpty (Typed Var))
  | TupleD (NonUnit Identifier)
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
  | IfElse Term Term Term
  -- Arrays
  | Select Term (NonEmpty Term)
  | Store Term (NonEmpty Term) Term
  -- Tuples
  | TupleLiteral (NonUnit Term)
  deriving (Show, Eq, Ord)

foldStore :: Foldable f => (a -> i -> a) -> (a -> i -> t -> t) -> a -> f i -> t -> t
foldStore select store a ii e
  | Just (i :| is) <- NE.nonEmpty (toList ii) = store a i (foldStore select store (select a i) is e)
  | otherwise = e

type Formula = Term

instance TypeOf Term where
  typeOf = \case
    IntegerConstant{} -> Integer
    BooleanConstant{} -> Boolean
    Variable        v -> typeOf v
    Constant        i -> typeOf i
    Application   f _ -> typeOf f
    Binary     op _ _ -> binaryOpRange op
    Unary        op _ -> unaryOpRange op
    Quantify{}        -> Boolean
    Equals{}          -> Boolean
    Let           _ t -> typeOf t
    IfElse      _ a _ -> typeOf a
    Select        a _ -> arrayElement (typeOf a)
    Store       a _ _ -> typeOf a
    TupleLiteral   es -> Tuple (fmap typeOf es)

newtype Conjunction = Conjunction { getConjunction :: Formula }
  deriving (Eq, Show)

instance Semigroup Conjunction where
  Conjunction x <> Conjunction y = Conjunction (x /\ y)
    where
      (/\) :: Formula -> Formula -> Formula
      f /\ BooleanConstant True = f
      BooleanConstant True /\ g = g
      -- This case is only needed to satisfy the associativity law of Semigroup
      Binary And f g /\ h = Binary And f (Binary And g h)
      f /\ g = Binary And f g

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
