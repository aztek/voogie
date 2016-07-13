module Kyckling.Program.AST where

import Kyckling.Theory
import qualified Kyckling.FOOL.AST as F

data LVal = Var String
          | ArrayElem String Expr
  deriving (Show, Eq)

data Expr = IntConst Integer
          | BoolConst Bool
          | Unary   UnaryOp  Expr
          | Binary  BinaryOp Expr Expr
          | Ternary          Expr Expr Expr
          | Eql   Expr Expr
          | InEql Expr Expr
          | LVal LVal
  deriving (Show, Eq)

data UpdateOp = Assign | Plus | Minus | Times
  deriving (Show, Eq)

data Stmt = If Expr [Stmt] [Stmt]
          | Declare Type [(String, Maybe Expr)]
          | Increment LVal
          | Decrement LVal
          | Update LVal UpdateOp Expr
          | Return Expr
  deriving (Show, Eq)

data FunDef = FunDef Type Name [Typed Name] [Stmt]
  deriving (Show, Eq)

data Assert = Assert F.Formula
  deriving (Show, Eq)

data AST = AST [FunDef] [Stmt] [Assert]
  deriving (Show, Eq)