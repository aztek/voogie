module Voogie.Boogie.AST where

import Data.List.NonEmpty (NonEmpty)

import Voogie.AST
import Voogie.Theory
import qualified Voogie.FOOL.AST as F

data LVal = Ref String [NonEmpty Expr]
  deriving (Show, Eq)

type Expr = AST Expr'
data Expr'
  = IntConst Integer
  | BoolConst Bool
  | Unary   UnaryOp  Expr
  | Binary  BinaryOp Expr Expr
  | Ternary          Expr Expr Expr
  | Equals Sign Expr Expr
  | LVal LVal
  deriving (Show, Eq)

type Stmt = AST Stmt'
data Stmt'
  = If Expr [Stmt] [Stmt]
  | Assign (NonEmpty (LVal, Expr))
  deriving (Show, Eq)

data Decl = Declare (Typed (NonEmpty String))
  deriving (Show, Eq)

data FunDef = FunDef Type String [Typed String] [Stmt]
  deriving (Show, Eq)

data Assume = Assume F.Formula
  deriving (Show, Eq)

data Returns = Returns (NonEmpty (Typed String))
  deriving (Show, Eq)

data Main = Main [String] [F.Formula] (Maybe Returns)
                 [Decl] [Either Stmt Assume] [F.Formula]
  deriving (Show, Eq)

data Boogie = Boogie [Decl] Main
  deriving (Show, Eq)
