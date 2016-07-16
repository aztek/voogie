module Kyckling.FOOL where

import Kyckling.FOOL.Tuple (Tuple)

import Kyckling.Theory

newtype Var = Var Name
  deriving (Show)

type Const = Typed Name

data Definition = Symbol Const [Typed Var]
                | TupleD (Tuple Const)
  deriving (Show)

data Binding = Binding Definition Term
  deriving (Show)

data Term = IntegerConstant Integer
          | BooleanConstant Bool
          | Variable (Typed Var)
          | Constant Const
          | Binary BinaryOp Term Term
          | Unary UnaryOp Term
          | Quantify Quantifier [Typed Var] Term
          | Equals Sign Term Term
          | Let Binding Term
          | If Term Term Term
          -- Arrays
          | Select Term Term
          | Store Term Term Term
          -- Tuples
          | TupleLiteral (Tuple Term)
          -- Maybe
          | Nothing_ Type
          | Just_ Term
          | IsJust Term
          | FromJust Term
          -- Either
          | Left_ Term Type
          | Right_ Type Term
          | IsLeft Term
          | FromLeft Term
          | FromRight Term
  deriving (Show)

type Formula = Term

instance TypeOf Term where
  typeOf (IntegerConstant _) = Integer
  typeOf (BooleanConstant _) = Boolean
  typeOf (Variable v) = typeOf v 
  typeOf (Constant c) = typeOf c
  typeOf (Binary op _ _) = binaryOpRange op
  typeOf (Unary  op _) = unaryOpRange op
  typeOf (Quantify{}) = Boolean
  typeOf (Equals{}) = Boolean
  typeOf (Let _ t) = typeOf t
  typeOf (If _ a _) = typeOf a

  typeOf (Select array _) = arrayArgument (typeOf array)
  typeOf (Store array _ _) = typeOf array

  typeOf (TupleLiteral args) = TupleType (fmap typeOf args)

  typeOf (Nothing_ t) = MaybeType t
  typeOf (Just_ t)    = MaybeType (typeOf t)
  typeOf (IsJust t)   = Boolean
  typeOf (FromJust t) = maybeArgument (typeOf t)

  typeOf (Left_  l t)  = EitherType (typeOf l) t
  typeOf (Right_ t r)  = EitherType t (typeOf r)
  typeOf (IsLeft _)    = Boolean
  typeOf (FromLeft  t) = leftArgument  (typeOf t)
  typeOf (FromRight t) = rightArgument (typeOf t)