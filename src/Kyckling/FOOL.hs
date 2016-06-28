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

typeOfTerm :: Term -> Type
typeOfTerm (IntegerConst _) = Integer
typeOfTerm (BooleanConst _) = Boolean
typeOfTerm (Variable v) = typeOf v 
typeOfTerm (Const c) = typeOf c
typeOfTerm (Select array _) = arrayArgument (typeOfTerm array)
typeOfTerm (Store array _ _) = typeOfTerm array
typeOfTerm (Binary op _ _) = binaryOpRange op
typeOfTerm (Unary  op _) = unaryOpRange op
typeOfTerm (Quantify{}) = Boolean
typeOfTerm (Eql   _ _) = Boolean
typeOfTerm (InEql _ _) = Boolean
typeOfTerm (Tuple _) = undefined
typeOfTerm (Let _ t) = typeOfTerm t
typeOfTerm (If _ a _) = typeOfTerm a