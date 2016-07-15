module Kyckling.FOOL where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Kyckling.Theory

newtype Var = Var Name
  deriving (Show)

type Constant = Typed Name

data Definition = Symbol Constant [Typed Var]
                | TupleD (NonEmpty Constant)
  deriving (Show)

data Binding = Binding Definition Term
  deriving (Show)

data Term = IntegerConst Integer
          | BooleanConst Bool
          | Variable (Typed Var)
          | Const Constant
          | Select Term Term
          | Store Term Term Term
          | Binary BinaryOp Term Term
          | Unary UnaryOp Term
          | Quantify Quantifier [Typed Var] Term
          | Eql   Term Term
          | InEql Term Term
          | Tuple (NonEmpty Term)
          | Left_ Term Type
          | Right_ Type Term
          | IsLeft Term
          | FromLeft Term
          | FromRight Term
          | Let Binding Term
          | If Term Term Term
  deriving (Show)

type Formula = Term

instance TypeOf Term where
  typeOf (IntegerConst _) = Integer
  typeOf (BooleanConst _) = Boolean
  typeOf (Variable v) = typeOf v 
  typeOf (Const c) = typeOf c
  typeOf (Select array _) = arrayArgument (typeOf array)
  typeOf (Store array _ _) = typeOf array
  typeOf (Binary op _ _) = binaryOpRange op
  typeOf (Unary  op _) = unaryOpRange op
  typeOf (Quantify{}) = Boolean
  typeOf (Eql   _ _) = Boolean
  typeOf (InEql _ _) = Boolean
  typeOf (Tuple args) = TupleType (NE.map typeOf args)
  typeOf (Left_  l t) = EitherType (typeOf l) t
  typeOf (Right_ t r) = EitherType t (typeOf r)
  typeOf (IsLeft _) = Boolean
  typeOf (FromLeft  t) = leftArgument  (typeOf t)
  typeOf (FromRight t) = rightArgument (typeOf t)
  typeOf (Let _ t) = typeOf t
  typeOf (If _ a _) = typeOf a