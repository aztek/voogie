module Voogie.Boogie where

import Data.List.NonEmpty

import Voogie.Theory
import qualified Voogie.FOOL as F

type Var = Typed Name
type Function = Typed Name

data LValue = LValue Var [NonEmpty Expression]
  deriving (Show, Eq)

lvariable :: LValue -> Var
lvariable (LValue var _) = var

instance TypeOf LValue where
  typeOf (LValue v is) = foldl (const . arrayElement) (typeOf v) is

data Expression
  = IntegerLiteral Integer
  | BooleanLiteral Bool
  | Ref LValue
  | Unary  UnaryOp    Expression
  | Binary BinaryOp   Expression Expression
  | IfElse Expression Expression Expression
  | FunApp Function [Expression]
  | Equals Sign Expression Expression
  deriving (Show, Eq)

instance TypeOf Expression where
  typeOf (IntegerLiteral _) = Integer
  typeOf (BooleanLiteral _) = Boolean
  typeOf (Ref lval) = typeOf lval
  typeOf (Unary op _) = unaryOpRange op
  typeOf (Binary op _ _) = binaryOpRange op
  typeOf (IfElse _ a _) = typeOf a
  typeOf (FunApp f _) = typeOf f
  typeOf Equals{} = Boolean

type Assignment = (LValue, Expression)

data Statement
  = Assign (NonEmpty Assignment)
  | If Expression Bool (NonEmpty Statement) [Statement]
  deriving (Show, Eq)

data Assume = Assume F.Formula
  deriving (Show, Eq)

type TopLevel = Either Statement Assume

data Main = Main [F.Formula] [TopLevel] [F.Formula]
  deriving (Show, Eq)

data Boogie = Boogie [Var] Main
  deriving (Show, Eq)
