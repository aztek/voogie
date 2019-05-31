{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Voogie.Theory where

import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Voogie.FOOL.Tuple as Tuple (nonUnit)
import Voogie.FOOL.Tuple (Tuple)

type Name = String

class Named t where
  nameOf :: t -> Name

instance Named Name where
  nameOf = id

data Type
  = Boolean
  | Integer
  | Array (NonEmpty Type) Type
  | TupleType (Tuple Type)
  | Functional (NonEmpty Type) Type
  | Custom Name
  deriving (Show, Eq, Ord)

arrayElement :: Type -> Type
arrayElement = \case
  Array _ r -> r
  t -> error (show t ++ " is not an array")

arrayArgument :: Type -> Type
arrayArgument = \case
  Array (_ :| is) r -> array is r
  t -> error (show t ++ " is not an array")

arrayIndexes :: Type -> [NonEmpty Type]
arrayIndexes = \case
  Array i r -> i : arrayIndexes r
  _ -> []

array :: [Type] -> Type -> Type
array [] s = s
array (t:ts) s = Array (t :| ts) s

tupleType :: NonEmpty Type -> Type
tupleType = either id TupleType . Tuple.nonUnit

returnType :: Type -> Type
returnType = \case
  Functional _ r -> r
  t -> error (show t ++ " is not a function")

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
  | Negative
  deriving (Show, Eq, Ord, Bounded, Enum)

data BinaryOp
  = And | Or | Imply | Iff
  | Greater | Less | Geq | Leq
  | Add | Subtract | Multiply | Divide
  deriving (Show, Eq, Ord, Bounded, Enum)

isAssociative :: BinaryOp -> Bool
isAssociative = \case
  And      -> True
  Or       -> True
  Imply    -> False
  Iff      -> False
  Greater  -> False
  Less     -> False
  Geq      -> False
  Leq      -> False
  Add      -> True
  Subtract -> False
  Multiply -> True
  Divide   -> False

unaryOpTypes :: UnaryOp -> (Type, Type)
unaryOpTypes = \case
  Negate   -> (Boolean, Boolean)
  Negative -> (Integer, Integer)

unaryOpDomain, unaryOpRange :: UnaryOp -> Type
unaryOpDomain = fst . unaryOpTypes
unaryOpRange  = snd . unaryOpTypes

logicalConnectives :: [BinaryOp]
logicalConnectives = [And, Or, Imply, Iff]

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
