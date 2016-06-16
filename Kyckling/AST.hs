module Kyckling.AST where

import Data.List
import Data.Maybe

data PrefixOp = Uminus | Uplus | Not
  deriving (Eq)

data InfixOp = Plus | Minus | Times
             | Less | Greater | Leq | Geq | Eq | NonEq
             | And | Or
  deriving (Eq)

data LVal = Var String
          | ArrayElem String Expr
  deriving (Eq)

data Expr = IntConst Integer
          | BoolConst Bool
          | Prefix PrefixOp Expr
          | Infix InfixOp Expr Expr
          | Ternary Expr Expr Expr
          | LVal LVal
  deriving (Eq)

data Type = I | B | Array Type
  deriving (Eq)

data UpdateOp = Assign | Add | Subtract | Multiply
  deriving (Eq)

data Stmt = If Expr [Stmt] [Stmt]
          | Declare Type [(String, Maybe Expr)]
          | Increment LVal
          | Decrement LVal
          | Update LVal UpdateOp Expr
  deriving (Eq)

data Assert = Assert Expr
  deriving (Eq)

data AST = AST [Stmt] [Assert]
  deriving (Eq)

instance Show PrefixOp where
  show Uminus = "-"
  show Uplus  = "+"
  show Not    = "!"

instance Show InfixOp where
  show Plus    = "+"
  show Minus   = "-"
  show Times   = "*"
  show Less    = "<"
  show Greater = ">"
  show Leq     = "<="
  show Geq     = ">="
  show Eq      = "=="
  show NonEq   = "!="
  show And     = "&&"
  show Or      = "||"

instance Show LVal where
  show (Var v) = v
  show (ArrayElem a e) = a ++ "[" ++ show e ++ "]"

instance Show Expr where
  show (IntConst i) = show i
  show (BoolConst True) = "true"
  show (BoolConst False) = "false"
  show (Prefix op e) = show op ++ show e
  show (Infix op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (Ternary c a b) = "(" ++ show c ++ " ? " ++ show a ++ " : " ++ show b ++ ")"
  show (LVal lval) = show lval

instance Show Type where
  show I = "int"
  show B = "bool"
  show (Array t) = show t ++ "[]"

instance Show UpdateOp where
  show Assign   = "="
  show Add      = "+="
  show Subtract = "-="
  show Multiply = "*="

showAtomic as = unwords as ++ ";\n"

instance Show Stmt where
  show (If c a b) = "if (" ++ show c ++ ") " ++ showBlock a ++ showElse b
    where
      showElse [] = ""
      showElse b  = " else " ++ showBlock b
      showBlock ss = "{\n" ++ concatMap show ss ++ "}\n"
  show (Declare t ds) = showAtomic [show t, intercalate ", " $ map showDef ds]
    where
      showDef (v, Nothing) = v
      showDef (v, Just e)  = v ++ " = " ++ show e
  show (Increment v) = showAtomic [show v ++ "++"]
  show (Decrement v) = showAtomic [show v ++ "--"]
  show (Update v op e) = showAtomic [show v, show op, show e]

instance Show Assert where
  show (Assert e) = showAtomic ["assert", show e]

instance Show AST where
  show (AST ss as) = concatMap show ss ++ "\n" ++ concatMap show as