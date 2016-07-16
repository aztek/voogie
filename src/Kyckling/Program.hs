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

data Expression = IntegerLiteral Integer
                | BooleanLiteral Bool
                | Ref LValue
                | Unary  UnaryOp    Expression
                | Binary BinaryOp   Expression Expression
                | IfElse Expression Expression Expression
                | Equals Sign Expression Expression
  deriving (Show)

instance TypeOf Expression where
  typeOf (IntegerLiteral _) = Integer
  typeOf (BooleanLiteral _) = Boolean
  typeOf (Ref lval) = typeOf lval
  typeOf (Unary op _) = unaryOpRange op
  typeOf (Binary op _ _) = binaryOpRange op
  typeOf (IfElse _ a _) = typeOf a
  typeOf (Equals{}) = Boolean

data Statement = Declare Var
               | Assign LValue Expression
               | If Expression [Statement] (Either [Statement] (Bool, TerminatingStatement))
  deriving (Show)

data TerminatingStatement = Return    [Statement] Expression
                          | IteReturn [Statement] Expression TerminatingStatement TerminatingStatement
  deriving (Show)

instance TypeOf TerminatingStatement where
  typeOf (Return    _ e) = typeOf e
  typeOf (IteReturn _ _ a _) = typeOf a

data Assertion = Assertion F.Formula
  deriving (Show)

data FunDef = FunDef Type Name [Typed Name] TerminatingStatement
  deriving (Show)

data Program = Program [FunDef] [Statement] [Assertion]
  deriving (Show)