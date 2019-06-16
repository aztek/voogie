{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module       : Voogie.Language
Description  : Abstract syntax shared between Boogie and FOOL.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.Language (
  Name,
  Named(..),
  Type(..),
  arrayElement,
  arrayArgument,
  arrayIndexes,
  array,
  tuple,
  returnType,
  Typed(..),
  valueOf,
  TypeOf(..),
  Quantifier(..),
  UnaryOp(..),
  BinaryOp(..),
  isAssociative,
  unaryOpTypes,
  unaryOpDomain,
  unaryOpRange,
  binaryOpTypes,
  binaryOpDomain,
  binaryOpRange,
  Sign(..)
) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonUnit (NonUnit)
import qualified Data.List.NonUnit as NonUnit (nonUnit)

type Name = String

class Named t where
  nameOf :: t -> Name

instance Named Name where
  nameOf = id

data Type
  = Boolean
  | Integer
  | Array (NonEmpty Type) Type
  | Tuple (NonUnit Type)
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

tuple :: NonEmpty Type -> Type
tuple = either id Tuple . NonUnit.nonUnit

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
  = And
  | Or
  | Imply
  | Iff
  | Greater
  | Less
  | Geq
  | Leq
  | Add
  | Subtract
  | Multiply
  | Divide
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

binaryOpTypes :: BinaryOp -> ((Type, Type), Type)
binaryOpTypes = \case
  And      -> boolean predicate
  Or       -> boolean predicate
  Imply    -> boolean predicate
  Iff      -> boolean predicate
  Greater  -> integer predicate
  Less     -> integer predicate
  Geq      -> integer predicate
  Leq      -> integer predicate
  Add      -> integer function
  Subtract -> integer function
  Multiply -> integer function
  Divide   -> integer function
  where
    boolean = (,) (Boolean, Boolean)
    integer = (,) (Integer, Integer)
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
