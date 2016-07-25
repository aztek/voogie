module Kyckling.Program where

import Kyckling.Theory
import qualified Kyckling.FOOL as F

type Var = Typed Name
type Function = Typed Name

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
                | FunApp Function [Expression]
                | Equals Sign Expression Expression
  deriving (Show)

instance TypeOf Expression where
  typeOf (IntegerLiteral _) = Integer
  typeOf (BooleanLiteral _) = Boolean
  typeOf (Ref lval) = typeOf lval
  typeOf (Unary op _) = unaryOpRange op
  typeOf (Binary op _ _) = binaryOpRange op
  typeOf (IfElse _ a _) = typeOf a
  typeOf (FunApp f _) = typeOf f
  typeOf (Equals{}) = Boolean

data Statement = Declare Var
               | Assign LValue Expression
               | If Expression NonTerminating (Either NonTerminating (Bool, Terminating))
  deriving (Show)

type NonTerminating = [Statement]

data Return = Return    Expression
            | IteReturn Expression Terminating Terminating
  deriving (Show)

data Terminating = Terminating NonTerminating Return
  deriving (Show)

instance TypeOf Return where
  typeOf (Return e) = typeOf e
  typeOf (IteReturn _ a _) = typeOf a

instance TypeOf Terminating where
  typeOf (Terminating _ r) = typeOf r

data Assertion = Assertion F.Formula
  deriving (Show)

data FunDef = FunDef Type Name [Typed Name] Terminating
  deriving (Show)

data Program = Program [FunDef] NonTerminating [Assertion]
  deriving (Show)