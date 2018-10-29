{-# LANGUAGE PatternGuards #-}

module Voogie.Theory where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Voogie.FOOL.Tuple as Tuple
import Voogie.FOOL.Tuple (Tuple)

type Name = String

data Type
  = Boolean
  | Integer
  | Array (NonEmpty Type) Type
  | TupleType (Tuple Type)
  | Functional (NonEmpty Type) Type
  | Custom Name
  deriving (Show, Eq, Ord)

arrayElement :: Type -> Type
arrayElement (Array _ r) = r
arrayElement t = error (show t ++ " is not an array")

arrayArgument :: Type -> Type
arrayArgument (Array (_ :| is) r)
  | Just is' <- NE.nonEmpty is = Array is' r
  | otherwise = r
arrayArgument t = error (show t ++ " is not an array")

arrayIndexes :: Type -> [NonEmpty Type]
arrayIndexes (Array i r) = i : arrayIndexes r
arrayIndexes _ = []

array :: [Type] -> Type -> Type
array [] s = s
array (t:ts) s = Array (t :| ts) s

tupleType :: NonEmpty Type -> Type
tupleType = either id TupleType . Tuple.nonUnit

returnType :: Type -> Type
returnType (Functional _ r) = r
returnType t = error (show t ++ " is not a function")

data Typed a = Typed Type a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

valueOf :: Typed a -> a
valueOf (Typed _ a) = a

class TypeOf a where
  typeOf :: a -> Type

instance TypeOf (Typed a) where
  typeOf (Typed t _) = t

data Quantifier
  = Forall
  | Exists
  deriving (Show, Eq, Ord, Bounded, Enum)

data UnaryOp
  = Negate
  | Positive
  | Negative
  deriving (Show, Eq, Ord, Bounded, Enum)

data BinaryOp
  = And | Or | Imply | Iff | Xor
  | Greater | Less | Geq | Leq
  | Add | Subtract | Multiply | Divide
  deriving (Show, Eq, Ord, Bounded, Enum)

unaryOpTypes :: UnaryOp -> (Type, Type)
unaryOpTypes = \case
  Negate   -> (Boolean, Boolean)
  Positive -> (Integer, Integer)
  Negative -> (Integer, Integer)

unaryOpDomain, unaryOpRange :: UnaryOp -> Type
unaryOpDomain = fst . unaryOpTypes
unaryOpRange  = snd . unaryOpTypes

logicalConnectives :: [BinaryOp]
logicalConnectives = [And, Or, Imply, Iff, Xor]

arithmeticPredicates :: [BinaryOp]
arithmeticPredicates = [Greater, Less, Geq, Leq]

arithmeticFunctions :: [BinaryOp]
arithmeticFunctions = [Add, Subtract, Multiply, Divide]

binaryOpTypes :: BinaryOp -> ((Type, Type), Type)
binaryOpTypes op
  | op `elem` logicalConnectives   = logical predicate
  | op `elem` arithmeticPredicates = arithmetic predicate
  | op `elem` arithmeticFunctions  = arithmetic function
  | otherwise = error "binaryOpTypes: unknown binary operator"
  where
    logical = (,) (Boolean, Boolean)
    arithmetic = (,) (Integer, Integer)
    function = Integer
    predicate = Boolean

binaryOpDomain :: BinaryOp -> (Type, Type)
binaryOpDomain = fst . binaryOpTypes

binaryOpRange :: BinaryOp -> Type
binaryOpRange = snd . binaryOpTypes

data Sign
  = Pos
  | Neg
  deriving (Show, Eq, Ord, Bounded, Enum)
