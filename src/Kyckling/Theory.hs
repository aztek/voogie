{-# LANGUAGE DeriveFunctor #-}

module Kyckling.Theory where

data Type = Boolean
          | Integer
          | Array Type
  deriving (Show, Eq)

arrayArgument :: Type -> Type
arrayArgument (Array t) = t

type Name = String

data Typed n = Typed n Type
  deriving (Show, Eq, Functor)

typeOf :: Typed a -> Type
typeOf (Typed _ t) = t

type Signature = [Typed Name]


data Quantifier = Forall | Exists
  deriving (Show, Eq)


data UnaryOp = Negate
             | Positive
             | Negative
  deriving (Show, Eq)

data BinaryOp = And | Or | Imply | Iff | Xor
              | Greater | Less | Geq | Leq
              | Add | Subtract | Multiply
  deriving (Show, Eq)

unaryOpTypes :: UnaryOp -> (Type, Type)
unaryOpTypes op =
  case op of
    Negate   -> (Boolean, Boolean)
    Positive -> (Integer, Integer)
    Negative -> (Integer, Integer)

unaryOpDomain, unaryOpRange :: UnaryOp -> Type
unaryOpDomain = fst . unaryOpTypes
unaryOpRange  = snd . unaryOpTypes


binaryOpTypes :: BinaryOp -> ((Type, Type), Type)
binaryOpTypes op =
  case op of
    And      -> ((Boolean, Boolean), Boolean)
    Or       -> ((Boolean, Boolean), Boolean)
    Imply    -> ((Boolean, Boolean), Boolean)
    Iff      -> ((Boolean, Boolean), Boolean)
    Xor      -> ((Boolean, Boolean), Boolean)
    Greater  -> ((Integer, Integer), Boolean)
    Less     -> ((Integer, Integer), Boolean)
    Geq      -> ((Integer, Integer), Boolean)
    Leq      -> ((Integer, Integer), Boolean)
    Add      -> ((Integer, Integer), Integer)
    Subtract -> ((Integer, Integer), Integer)
    Multiply -> ((Integer, Integer), Integer)

binaryOpDomain :: BinaryOp -> (Type, Type)
binaryOpDomain = fst . binaryOpTypes

binaryOpRange :: BinaryOp -> Type
binaryOpRange = snd . binaryOpTypes