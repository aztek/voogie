module Kyckling.Program where

import Kyckling.Theory
import qualified Kyckling.FOOL as F

type Var = Typed Name

data LValue = Variable Var
            | ArrayElem Var Expression
  deriving (Show)

instance TypeOf LValue where
  typeOf (Variable  v) = typeOf v
  typeOf (ArrayElem v _) = arrayArgument (typeOf v)

data Expression = IntegerConst Integer
                | BoolConst Bool
                | Ref LValue
                | Unary  UnaryOp    Expression
                | Binary BinaryOp   Expression Expression
                | IfElse Expression Expression Expression
                | Eql    Expression Expression
                | InEql  Expression Expression
  deriving (Show)

instance TypeOf Expression where
  typeOf (IntegerConst _) = Integer
  typeOf (BoolConst    _) = Boolean
  typeOf (Ref lval) = typeOf lval
  typeOf (Unary op _) = unaryOpRange op
  typeOf (Binary op _ _) = binaryOpRange op
  typeOf (IfElse _ a _) = typeOf a
  typeOf (Eql{})   = Boolean
  typeOf (InEql{}) = Boolean

data Statement = Declare Var
               | Assign LValue Expression
               | If Expression [Statement] [Statement]
  deriving (Show)

data Assertion = Assertion F.Formula
  deriving (Show)

data Program = Program [Statement] [Assertion]
  deriving (Show)