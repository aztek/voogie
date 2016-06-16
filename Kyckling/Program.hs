module Kyckling.Program (
  Type(..),
  Var(..),
  UnaryOp(..), unaryOpDomain, unaryOpRange,
  BinaryOp(..), binaryOpDomain, binaryOpRange,
  TernaryOp(..),
  LValue(..),
  Expression(..),
  Statement(..),
  Assertion(..),
  Program(..)
) where

data Type = Integer
          | Boolean
          | Array Type
  deriving (Show, Eq)

data Var = Var String Type
  deriving (Show)

data UnaryOp = Negate
             | Positive
             | Negative
  deriving (Show)

unaryOpDomain :: UnaryOp -> Type
unaryOpDomain op = 
  case op of
    Negate   -> Boolean
    Positive -> Integer
    Negative -> Integer

unaryOpRange :: UnaryOp -> Type
unaryOpRange op = 
  case op of
    Negate   -> Boolean
    Positive -> Integer
    Negative -> Integer


data BinaryOp = And | Or
              | Greater | Less | Geq | Leq
              | Add | Subtract | Multiply
  deriving (Show)

binaryOpDomain :: BinaryOp -> (Type, Type)
binaryOpDomain op =
  case op of
    And      -> (Boolean, Boolean)
    Or       -> (Boolean, Boolean)
    Greater  -> (Integer, Integer)
    Less     -> (Integer, Integer)
    Geq      -> (Integer, Integer)
    Leq      -> (Integer, Integer)
    Add      -> (Integer, Integer)
    Subtract -> (Integer, Integer)
    Multiply -> (Integer, Integer)

binaryOpRange :: BinaryOp -> Type
binaryOpRange op =
  case op of
    And      -> Boolean
    Or       -> Boolean
    Greater  -> Boolean
    Less     -> Boolean
    Geq      -> Boolean
    Leq      -> Boolean
    Add      -> Integer
    Subtract -> Integer
    Multiply -> Integer

data TernaryOp = IfElse
  deriving (Show)

data LValue = Variable Var
            | ArrayElem Var Expression
  deriving (Show)

data Expression = IntegerConst Integer
                | BoolConst Bool
                | Unary   UnaryOp   Expression
                | Binary  BinaryOp  Expression Expression
                | Ternary TernaryOp Expression Expression Expression
                | Eql Expression Expression
                | InEql Expression Expression
                | Ref LValue
  deriving (Show)

data Statement = Declare Var (Maybe Expression)
               | Assign LValue Expression
               | If Expression [Statement] [Statement]
  deriving (Show)

data Assertion = Assertion Expression
  deriving (Show)

data Program = Program [Statement] [Assertion]
  deriving (Show)