{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Voogie.Theory where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Voogie.FOOL.Tuple as Tuple
import Voogie.FOOL.Tuple (Tuple)

data Type = Boolean
          | Integer
          | Array (NonEmpty Type) Type
          | TupleType (Tuple Type)
  deriving (Show, Eq, Ord)

arrayElement :: Type -> Type
arrayElement (Array _ r) = r
arrayElement t = error (show t ++ " is not an array")

arrayArgument :: Type -> Type
arrayArgument (Array (_ :| is) r) =
  case NE.nonEmpty is of
    Nothing  -> r
    Just is' -> Array is' r
arrayArgument t = error (show t ++ " is not an array")

arrayIndexes :: Type -> [NonEmpty Type]
arrayIndexes (Array i r) = i : arrayIndexes r
arrayIndexes _ = []

tupleType :: NonEmpty Type -> Type
tupleType = either id TupleType . Tuple.nonUnit

type Name = String

data Typed a = Typed Type a
  deriving (Show, Eq, Ord, Functor, Foldable)

instance Traversable Typed where
  traverse f (Typed t a) = Typed t <$> f a

class TypeOf a where
  typeOf :: a -> Type

instance TypeOf (Typed a) where
  typeOf (Typed t _) = t

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
    And      -> logical predicate
    Or       -> logical predicate
    Imply    -> logical predicate
    Iff      -> logical predicate
    Xor      -> logical predicate

    Greater  -> arithmetic predicate
    Less     -> arithmetic predicate
    Geq      -> arithmetic predicate
    Leq      -> arithmetic predicate

    Add      -> arithmetic function
    Subtract -> arithmetic function
    Multiply -> arithmetic function
    Divide   -> arithmetic function
  where
    logical = (,) (Boolean, Boolean)
    arithmetic = (,) (Integer, Integer)
    function = Integer
    predicate = Boolean

binaryOpDomain :: BinaryOp -> (Type, Type)
binaryOpDomain = fst . binaryOpTypes

binaryOpRange :: BinaryOp -> Type
binaryOpRange = snd . binaryOpTypes

data Sign = Pos | Neg
  deriving (Show, Eq)
