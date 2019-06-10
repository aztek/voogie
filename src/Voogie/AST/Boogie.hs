module Voogie.AST.Boogie (
  Identifier,
  LValue(..),
  Expression,
  ExpressionF(..),
  Assignment,
  Statement,
  StatementF(..),
  Declaration(..),
  FunDef(..),
  Property(..),
  TopLevel,
  Returns(..),
  Main(..),
  Boogie(..)
) where

import Data.List.NonEmpty (NonEmpty)

import Voogie.AST
import qualified Voogie.AST.FOOL as F
import Voogie.Pretty.Boogie (Pretty(pretty), empty)
import Voogie.Language

type Identifier = AST Name

data LValue = LValue Identifier [NonEmpty Expression]
  deriving (Show, Eq)

type Expression = AST ExpressionF
data ExpressionF
  = IntegerLiteral Integer
  | BooleanLiteral Bool
  | Ref LValue
  | Unary UnaryOp Expression
  | Binary BinaryOp Expression Expression
  | IfElse Expression Expression Expression
  | Equals Sign Expression Expression
  deriving (Show, Eq)

type Assignment = (LValue, Expression)

type Statement = AST StatementF
data StatementF
  = If Expression [Statement] [Statement]
  | Assign (NonEmpty (LValue, Expression))
  deriving (Show, Eq)

data Declaration = Declaration { getDeclaration :: Typed (NonEmpty Identifier) }
  deriving (Show, Eq)

data FunDef = FunDef Type Identifier [Typed Identifier] [Statement]
  deriving (Show, Eq)

data Property
  = Assume F.Formula
  | Assert F.Formula
  deriving (Show, Eq)

type TopLevel = Either Statement Property

data Returns = Returns { getReturns :: NonEmpty (Typed Identifier) }
  deriving (Show, Eq)

data Main = Main [Identifier] [F.Formula] (Maybe Returns)
                 [Declaration] [TopLevel] [F.Formula]
  deriving (Show, Eq)

data Boogie = Boogie [Declaration] Main
  deriving (Show, Eq)

instance Pretty Boogie where
  pretty = const empty
