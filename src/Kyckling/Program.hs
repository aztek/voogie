{-# LANGUAGE DeriveFunctor #-}

module Kyckling.Program where

import Data.List.NonEmpty

import Kyckling.Theory
import qualified Kyckling.FOOL as F

type Var = Typed Name
type Function = Typed Name

data LValue = Variable Var
            | ArrayElem Var Expression
  deriving (Show)

lvariable :: LValue -> Var
lvariable (Variable  v)   = v
lvariable (ArrayElem v _) = v

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

data Statement = Assign (NonEmpty (LValue, Expression))
               | If Expression NonTerminating (Either NonTerminating (Bool, Terminating))
  deriving (Show)

data Scoped a = Scoped [Var] a
  deriving (Show, Functor)

appendScope :: [Var] -> Scoped a -> Scoped a
appendScope vars (Scoped scope a) = Scoped (vars ++ scope) a

type NonTerminating = [Scoped Statement]

data Return = Return    Expression
            | IteReturn Expression Terminating Terminating
  deriving (Show)

data Terminating = Terminating NonTerminating (Scoped Return)
  deriving (Show)

instance TypeOf Return where
  typeOf (Return e) = typeOf e
  typeOf (IteReturn _ a _) = typeOf a

instance TypeOf Terminating where
  typeOf (Terminating _ r) = typeOf r

instance TypeOf a => TypeOf (Scoped a) where
  typeOf (Scoped _ a) = typeOf a

data Assertion = Assertion F.Formula
  deriving (Show)

data FunDef = FunDef (Typed Name) [Var] Terminating
  deriving (Show)

data Program = Program [FunDef] NonTerminating [Assertion]
  deriving (Show)