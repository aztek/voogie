module Voogie.FOOL where

import Voogie.FOOL.Tuple (Tuple)

import Voogie.Theory

newtype Var = Var Name
  deriving (Show, Eq)

type Identifier = Typed Name

data Definition = Symbol Identifier [Typed Var]
                | TupleD (Tuple Identifier)
  deriving (Show, Eq)

data Binding = Binding Definition Term
  deriving (Show, Eq)

data Term = IntegerConstant Integer
          | BooleanConstant Bool
          | Variable (Typed Var)
          | Application Identifier [Term]
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
          -- Option
          | None Type
          | Some Term
          | IsSome Term
          | FromSome Term
          -- Either
          | Left_ Term Type
          | Right_ Type Term
          | IsLeft Term
          | FromLeft Term
          | FromRight Term
  deriving (Show, Eq)

type Formula = Term

instance TypeOf Term where
  typeOf (IntegerConstant _) = Integer
  typeOf (BooleanConstant _) = Boolean
  typeOf (Variable v) = typeOf v 
  typeOf (Application i _) = typeOf i
  typeOf (Binary op _ _) = binaryOpRange op
  typeOf (Unary  op _) = unaryOpRange op
  typeOf (Quantify{}) = Boolean
  typeOf (Equals{}) = Boolean
  typeOf (Let _ t) = typeOf t
  typeOf (If _ a _) = typeOf a

  typeOf (Select array _) = arrayArgument (typeOf array)
  typeOf (Store array _ _) = typeOf array

  typeOf (TupleLiteral args) = TupleType (fmap typeOf args)

  typeOf (None t)     = OptionType t
  typeOf (Some t)     = OptionType (typeOf t)
  typeOf (IsSome t)   = Boolean
  typeOf (FromSome t) = optionArgument (typeOf t)

  typeOf (Left_  l t)  = EitherType (typeOf l) t
  typeOf (Right_ t r)  = EitherType t (typeOf r)
  typeOf (IsLeft _)    = Boolean
  typeOf (FromLeft  t) = leftArgument  (typeOf t)
  typeOf (FromRight t) = rightArgument (typeOf t)
