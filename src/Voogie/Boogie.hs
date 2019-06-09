{-# LANGUAGE LambdaCase #-}

module Voogie.Boogie (
  module Voogie.Boogie,
  module Voogie.Theory
) where

import Data.List.NonEmpty (NonEmpty)

import qualified Voogie.FOOL as F
import Voogie.Theory

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
  | Unary UnaryOp Expression
  | Binary BinaryOp Expression Expression
  | IfElse Expression Expression Expression
  | Equals Sign Expression Expression
  | FunApp Function (NonEmpty Expression)
  deriving (Show, Eq)

instance TypeOf Expression where
  typeOf = \case
    IntegerLiteral _ -> Integer
    BooleanLiteral _ -> Boolean
    Ref           lv -> typeOf lv
    Unary       op _ -> unaryOpRange op
    Binary    op _ _ -> binaryOpRange op
    IfElse     _ a _ -> typeOf a
    FunApp       f _ -> returnType (typeOf f)
    Equals{}         -> Boolean

type Assignment = (LValue, Expression)

data Statement
  = Assign (NonEmpty Assignment)
  | If Expression Bool (NonEmpty Statement) [Statement]
  deriving (Show, Eq)

data Property
  = Assume F.Formula
  | Assert F.Formula
  deriving (Show, Eq)

type TopLevel = Either Statement Property

data Main = Main {
  modifies :: [Name],
  requires :: [F.Formula],
  contents :: [TopLevel],
  ensures  :: [F.Formula]
} deriving (Show, Eq)

data Boogie = Boogie [Var] Main
  deriving (Show, Eq)
