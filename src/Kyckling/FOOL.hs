module Kyckling.FOOL where

import Kyckling.Type

data Fun = Sum
         | Difference
         | Product
         | Uminus
         | Lesseq
         | Less
         | Greatereq
         | Greater
         | Select
         | Store
         | Tuple
  deriving (Show)

data BinaryOp = Impl
              | And
              | Or
              | Eq
              | InEq
  deriving (Show)

data UnaryOp = Not
  deriving (Show)

type Var = String

type Constant = (String, Type)

data TypedVar = TypedVar Var Type
  deriving (Show)

data Definition = Symbol Constant [TypedVar]
                | TupleD [Constant]
  deriving (Show)

data Binding = Binding Definition Term
  deriving (Show)

data Quantifier = Forall | Exists
  deriving (Show)

data Term = IntegerConst Integer
          | BooleanConst Bool
          | FunApp Fun [Term]
          | Var Var
          | Const Constant
          | Unary  UnaryOp  Term
          | Binary BinaryOp Term Term
          | Quantify Quantifier [TypedVar] Term
          | Let Binding Term
          | If Term Term Term
  deriving (Show)

type Formula = Term