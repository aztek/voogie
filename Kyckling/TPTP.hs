module Kyckling.TPTP where

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
type Constant = String

data Sort = Boolean
          | Integer
          | TupleS [Sort]
          | Array Sort Sort
  deriving (Show)

data SortedVar = SortedVar Var Sort
  deriving (Show)

data Definition = Symbol Constant [SortedVar]
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
          | Constant Constant
          | Unary  UnaryOp  Term
          | Binary BinaryOp Term Term
          | Quantify Quantifier [SortedVar] Term
          | Let Binding Term
          | If Term Term Term
  deriving (Show)

data SortDeclaration = SortDeclaration Constant Sort
  deriving (Show)

data Conjecture = Conjecture String Term
  deriving (Show)

data TPTP = TPTP [SortDeclaration] Conjecture
  deriving (Show)