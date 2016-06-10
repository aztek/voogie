{-#LANGUAGE GADTs #-}

module Kyckling.Program where

import Text.Printf
import Data.List

data Type = Int | Bool | Array Type

data Connective = And | Or | Iff

instance Show Connective where
  show And = "&&"
  show Or  = "||"
  show Iff = "=="

data Comparison = Eq | Ineq | Geq | Leq | G | L

instance Show Comparison where
  show Eq   = "=="
  show Ineq = "!="
  show Geq  = ">="
  show Leq  = "<="
  show G    = ">"
  show L    = "<"

data ArithOp = Plus | Minus

instance Show ArithOp where
  show Plus  = "+"
  show Minus = "-"

type Var = String
type Env = [(Var, Type)]

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool

  Not :: Expr Bool -> Expr Bool
  Conn :: Connective -> Expr Bool -> Expr Bool -> Expr Bool
  Compare :: Comparison -> Expr Int  -> Expr Int  -> Expr Bool

  Op :: ArithOp -> Expr Int -> Expr Int -> Expr Int

  SelectI :: Var -> Expr Int -> Expr Int
  SelectB :: Var -> Expr Int -> Expr Bool

  VarI  :: Var -> Expr Int
  VarB  :: Var -> Expr Bool
  VarIA :: Var -> Expr Int
  VarBA :: Var -> Expr Bool

  Tern :: Expr Bool -> Expr a -> Expr a -> Expr a

instance Show (Expr a) where
  show (I i) = show i
  show (B True) = "true"
  show (B False) = "false"
  show (Not e) = "!" ++ show e
  show (Conn c e1 e2) = show e1 ++ " " ++ show c ++ " " ++ show e2
  show (Compare c e1 e2) = show e1 ++ " " ++ show c ++ " " ++ show e2
  show (Op o e1 e2) = show e1 ++ " " ++ show o ++ " " ++ show e2
  show (SelectI a i) = show a ++ "[" ++ show i ++ "]"
  show (SelectB a i) = show a ++ "[" ++ show i ++ "]"
  show (VarI  v) = v
  show (VarB  v) = v
  show (VarIA v) = v
  show (VarBA v) = v
  show (Tern c a b) = show c ++ " ? " ++ show a ++ " : " ++ show b

data Statement where
  IfElse :: Expr Bool -> Statement -> Statement -> Statement
  If :: Expr Bool -> Statement -> Statement
  (:=) :: [Var] -> [Expr a] -> Statement
  Seq :: [Statement] -> Statement

toIndent :: Int -> String
toIndent n = replicate (n * 2) ' '

els :: Int -> String
els n = rbra n ++ toIndent n ++ "else" ++ "\n" ++ lbra n

lbra :: Int -> String
lbra n = toIndent n ++ "{" ++ "\n"

rbra :: Int -> String
rbra n = toIndent n ++ "}" ++ "\n"

semicolon :: String
semicolon = ";" ++ "\n"

condition :: Int -> String -> String
condition n c = toIndent n ++ "if (" ++ c ++ ") " ++ lbra n

showIndented :: Int -> Statement -> String
showIndented n (If c a) = condition n (show c) ++ showIndented (n + 1) a ++ rbra n
showIndented n (IfElse c a b) = condition n (show c) ++ showIndented (n + 1) a ++
                                els n ++ showIndented (n + 1) b ++ rbra n
showIndented n (vs := es) = toIndent n ++ intercalate ", " vs ++ " = " ++ intercalate ", " (map show es) ++ semicolon
showIndented n (Seq ss) = concatMap (showIndented n) ss

instance Show Statement where
  show = showIndented 0

data Assertion = Assertion (Expr Bool)
instance Show Assertion where
  show (Assertion e) = "assert(" ++ show e ++ ");"

data Program = Program Statement [Assertion]
instance Show Program where
  show (Program s as) = show s ++ "\n" ++ intercalate "\n" (map show as)