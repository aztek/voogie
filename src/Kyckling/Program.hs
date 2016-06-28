module Kyckling.Program where

import Kyckling.Theory
import qualified Kyckling.FOOL as F

type Var = Typed Name

data LValue = Variable Var
            | ArrayElem Var Expression
  deriving (Show)

data Expression = IntegerConst Integer
                | BoolConst Bool
                | Ref LValue
                | Unary  UnaryOp    Expression
                | Binary BinaryOp   Expression Expression
                | IfElse Expression Expression Expression
                | Eql    Expression Expression
                | InEql  Expression Expression
  deriving (Show)

data Statement = Declare Var
               | Assign LValue Expression
               | If Expression [Statement] [Statement]
  deriving (Show)

data Assertion = Assertion F.Formula
  deriving (Show)

data Program = Program [Statement] [Assertion]
  deriving (Show)