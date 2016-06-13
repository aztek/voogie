module Kyckling.AST where

import Data.List
import Data.Maybe

data PrefixOp = Uminus | Not

data InfixOp = Plus | Minus | Times | Divide
             | Less | Greater | Leq | Geq | Eq
             | And | Or

data LVal = Var String
          | ArrayElem String Expr

data Expr = IntConst Integer
          | BoolConst Bool
          | Prefix PrefixOp Expr
          | Infix InfixOp Expr Expr
          | Ternary Expr Expr Expr
          | LVal LVal

data Type = I | B | Array Type

data Stmt = Assign LVal Expr
          | If Expr Stmt (Maybe Stmt)
          | Block [Stmt]
          | Declare Type String (Maybe Expr)
          | Increment LVal
          | Decrement LVal
          | Assert Expr

data AST = AST [Stmt]

instance Show PrefixOp where
  show Uminus = "-"
  show Not    = "!"

instance Show InfixOp where
  show Plus    = "+"
  show Minus   = "-"
  show Times   = "*"
  show Divide  = "/"
  show Less    = "<"
  show Greater = ">"
  show Leq     = "<="
  show Geq     = ">="
  show Eq      = "=="
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

instance Show Stmt where
  show (Assign v e) = show v ++ " = " ++ show e ++ ";\n"
  show (If e s1 s2) = "if (" ++ show e ++ ") " ++ show s1 ++ if isJust s2 then " else " ++ show (fromJust s2) else ""
  show (Block ss) = "{\n" ++ concatMap show ss ++ "}\n"
  show (Declare t v e) = show t ++ " " ++ v ++ (if isJust e then " = " ++ show (fromJust e) else "") ++ ";\n"
  show (Increment v) = show v ++ "++;\n"
  show (Decrement v) = show v ++ "--;\n"
  show (Assert e) = "assert " ++ show e ++ ";\n"

instance Show AST where
  show (AST ss) = concatMap show ss