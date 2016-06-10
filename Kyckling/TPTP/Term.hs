{-#LANGUAGE GADTs #-}

module Kyckling.TPTP.Term (
  Fun (Select, Store, Sum, Uminus, FCustom),
  Sort (Individual, Boolean, Integer, Real, STuple, Array, SCustom),
  SortedVar (SortedVar),
  Definition (Symbol, TupleD),
  Binding (Binding),
  Term (FunApp, Var, Forall, Exists, Tuple, Let)
) where

import Data.List

list :: Show a => [a] -> String
list = intercalate ", " . map show

data Fun = Select | Store | Sum | Uminus | FCustom String
instance Show Fun where
  show Select = "$select"
  show Store  = "$store"
  show Sum    = "$sum"
  show Uminus = "$uminus"
  show (FCustom f) = f

type Var  = String

data Sort = Individual | Boolean | Integer | Real | STuple [Sort] | Array Sort | SCustom String
instance Show Sort where
  show Individual  = "$i"
  show Boolean     = "$o"
  show Integer     = "$int"
  show Real        = "$real"
  show (Array s)   = "$array($int, " ++ show s ++ ")"
  show (STuple ss) = "$tuple(" ++ list ss ++ ")"
  show (SCustom s) = s


data SortedVar = SortedVar { var :: Var , sort :: Sort }
instance Show SortedVar where
  show (SortedVar v s) = show v ++ ":" ++ show s


data Definition where
  Symbol :: Fun -> [SortedVar] -> Definition
  TupleD :: [Fun] -> Definition

instance Show Definition where
  show (Symbol f []) = show f
  show (Symbol f vs) = "![" ++ list vs ++ "]: " ++ show f ++ "(" ++ list (map var vs) ++ ")"
  show (TupleD args) = "[" ++ list args ++ "]"


data Binding = Binding Definition Term
instance Show Binding where
  show (Binding b s) = show b ++ " := " ++ show s


data Term where
  FunApp :: Fun -> [Term] -> Term
  Var :: Var -> Term
  Forall :: [SortedVar] -> Term -> Term
  Exists :: [SortedVar] -> Term -> Term
  Tuple :: [Term] -> Term
  Let :: [Binding] -> Term -> Term

instance Show Term where
  show (FunApp f []) = show f
  show (FunApp f ts) = show f ++ "(" ++ list ts ++ ")"
  show (Var v) = v

  show (Forall [] t) = show t
  show (Forall vs t) = "![" ++ list vs ++ "]: (" ++ show t ++ ")"

  show (Exists [] t) = show t
  show (Exists vs t) = "?[" ++ list vs ++ "]: (" ++ show t ++ ")"

  show (Tuple args) = "[" ++ list args ++ "]"

  show (Let [] t) = show t
  show (Let [b] t) = "$let(" ++ show b ++ ", " ++ show t ++ ")"
  show (Let bs  t) = "$let([" ++ list bs ++ "], " ++ show t ++ ")"