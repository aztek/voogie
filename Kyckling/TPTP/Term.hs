{-#LANGUAGE GADTs #-}

module TPTP.Term where

import Data.List

type Fun  = String
type Var  = String
type Sort = String

data Definition where
  Symbol :: Fun -> [(Var, Sort)] -> Definition
  TupleD :: [Fun] -> Definition

instance Show Definition where
  show (Symbol f []) = f
  show (Symbol f vs) = "![" ++ map (\(v, s) -> v ++ ":" ++ s) vs ++ "]: " ++ f ++ "(" ++ intercalate ", " (map fst vs) ++ ")"
  show (TupleD args) = "[" ++ intercalate ", " (map show args) ++ "]"

data Term where
  FunApp :: Fun -> [Term] -> Term
  Var :: Var -> Term

  Forall :: [(Var, Sort)] -> Term -> Term
  Exists :: [(Var, Sort)] -> Term -> Term

  Tuple :: [Term] -> Term

  Let :: [(Definition, Term)] -> Term -> Term

instance Show Term where
  show (FunApp f []) = f
  show (FunApp f ts) = f ++ "(" ++ intercalate ", " (map show ts) ++ ")"
  show (Var v) = v

  show (Forall vs t) = "![" ++ intercalate ", " (map (\(v, s) -> v ++ ":" ++ s) vs) ++ "]: (" ++ show t ++ ")"
  show (Exists vs t) = "?[" ++ intercalate ", " (map (\(v, s) -> v ++ ":" ++ s) vs) ++ "]: (" ++ show t ++ ")"

  show (Tuple args) = "[" ++ intercalate ", " (map show args) ++ "]"

  show (Let [] t) = show t
  show (Let [(b, s)] t) = "$let(" ++ show b ++ " := " ++ show s ++ ", " ++ show t ++ ")"
  show (Let bs t) = "$let([" ++ intercalate ", " (map showBinding bs) ++ "], " ++ show t ++ ")"
    where showBinding (b, s) = show b ++ " := " ++ show s

