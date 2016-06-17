module Kyckling.TPTP where

data Fun = Sum
         | Difference
         | Product
         | Divide
         | Uminus
         | Lesseq
         | Less
         | Greatereq
         | Greater
         | Select
         | Store
         | Tuple

data BinaryOp = Impl
              | Xor
              | Iff
              | And
              | Or

data UnaryOp = Not

type Var  = String

data Sort = Individual
          | Boolean
          | Integer
          | STuple [Sort]
          | Array Sort Sort

data SortedVar = SortedVar Var Sort

data Definition = Symbol Fun [SortedVar]
                | TupleD [Fun]

data Binding = Binding Definition Term

data Quantifier = Forall | Exists

data Term = FunApp Fun [Term]
          | Var Var
          | Binary BinaryOp Term Term
          | Quantify Quantifier [SortedVar] Term
          | Let [Binding] Term
          | If Term Term Term

data InputType = Axiom | Conjecture

data TPTP = TPTP String InputType Term