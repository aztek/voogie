{-# LANGUAGE DeriveFunctor #-}

module Voogie.Theory where

import Data.Set
import Data.List.NonEmpty (NonEmpty)

import qualified Voogie.FOOL.Tuple as Tuple
import Voogie.FOOL.Tuple (Tuple)

data Type = Boolean
          | Integer
          | Array Type Type
          | TupleType (Tuple Type)
          | OptionType Type
          | EitherType Type Type
  deriving (Show, Eq, Ord)

isArray :: Type -> Bool
isArray (Array _ _) = True
isArray _ = False

arrayArgument :: Type -> Type
arrayArgument (Array _ t) = t
arrayArgument t = error (show t ++ " is not an array")

isOption :: Type -> Bool
isOption (OptionType _) = True
isOption _ = False

optionArgument :: Type -> Type
optionArgument (OptionType t) = t
optionArgument t = error (show t ++ " is not an option")

isEither :: Type -> Bool
isEither (EitherType{}) = True
isEither _ = False

eitherArguments :: Type -> (Type, Type)
eitherArguments (EitherType l r) = (l, r)
eitherArguments t = error (show t ++ " is not a disjoint union")

leftArgument, rightArgument :: Type -> Type
leftArgument  = fst . eitherArguments
rightArgument = snd . eitherArguments

tupleType :: NonEmpty Type -> Type
tupleType = either id TupleType . Tuple.nonUnit

type Name = String

data Typed n = Typed n Type
  deriving (Show, Eq, Ord, Functor)

type Signature = Set (Typed Name)

class TypeOf a where
  typeOf :: a -> Type

instance TypeOf (Typed a) where
  typeOf (Typed _ t) = t

data Quantifier = Forall | Exists
  deriving (Show, Eq)


data UnaryOp = Negate
             | Positive
             | Negative
  deriving (Show, Eq)

data BinaryOp = And | Or | Imply | Iff | Xor
              | Greater | Less | Geq | Leq
              | Add | Subtract | Multiply | Divide
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
    Divide   -> ((Integer, Integer), Integer)

binaryOpDomain :: BinaryOp -> (Type, Type)
binaryOpDomain = fst . binaryOpTypes

binaryOpRange :: BinaryOp -> Type
binaryOpRange = snd . binaryOpTypes

data Sign = Pos | Neg
  deriving (Show, Eq)
