module Voogie.Boogie.AST where

import Voogie.FOOL.Tuple

import Voogie.Theory
import qualified Voogie.FOOL.AST as F

data LVal = Var String
          | ArrayElem String Expr
  deriving (Show, Eq)

data Expr = IntConst Integer
          | BoolConst Bool
          | Unary   UnaryOp  Expr
          | Binary  BinaryOp Expr Expr
          | Ternary          Expr Expr Expr
          | FunApp String [Expr]
          | Equals Sign Expr Expr
          | LVal LVal
  deriving (Show, Eq)

data UpdateOp = Assign | Plus | Minus | Times
  deriving (Show, Eq)

data Stmt = If Expr [Stmt] [Stmt]
          | Increment LVal
          | Decrement LVal
          | Update LVal UpdateOp Expr
          | Return Expr
  deriving (Show, Eq)

data Declaration = Declare (Typed [String])
  deriving (Show, Eq)

data FunDef = FunDef Type String [Typed String] [Stmt]
  deriving (Show, Eq)

--data Assert = Assert F.Formula
--  deriving (Show, Eq)

data Returns = Returns (Typed String)
  deriving (Show, Eq)

data Main = Main [F.Formula] (Maybe Returns) [Declaration] [Stmt] [F.Formula]
  deriving (Show, Eq)

data AST = AST [Declaration] Main
  deriving (Show, Eq)
