module Voogie.Boogie.AST where

import Data.List.NonEmpty (NonEmpty)

import Voogie.AST
import Voogie.Theory
import qualified Voogie.FOOL.AST as F

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

type Identifier = AST String

data LVal = Ref Identifier [NonEmpty Expr]
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

data Decl = Declare (Typed (NonEmpty Identifier))
  deriving (Show, Eq)

data FunDef = FunDef Type Identifier [Typed Identifier] [Stmt]
  deriving (Show, Eq)

data Assume = Assume F.Formula
  deriving (Show, Eq)

data Returns = Returns (NonEmpty (Typed Identifier))
  deriving (Show, Eq)

data Main = Main [Identifier] [F.Formula] (Maybe Returns)
                 [Decl] [Either Stmt Assume] [F.Formula]
  deriving (Show, Eq)

data Boogie = Boogie [Decl] Main
  deriving (Show, Eq)

instance Pretty Boogie where
  pretty = const empty
