module Kyckling.FOOL where

import Kyckling.Theory

newtype Var = Var Name
  deriving (Show)

type Constant = Typed Name

data Definition = Symbol Constant [Typed Var]
                | TupleD [Constant]
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
          | Tuple [Term]
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
  typeOf (Tuple _) = undefined
  typeOf (Let _ t) = typeOf t
  typeOf (If _ a _) = typeOf a