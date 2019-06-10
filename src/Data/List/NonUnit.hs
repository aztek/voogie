{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.List.NonUnit (
  NonUnit(..),
  toNonEmpty,
  nonUnit,
) where

import qualified Data.List.NonEmpty as NE (cons, nonEmpty, NonEmpty(..))
import Data.List.NonEmpty (NonEmpty)

data NonUnit a = a :| NonEmpty a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

toNonEmpty :: NonUnit a -> NonEmpty a
toNonEmpty (a :| ne) = NE.cons a ne

nonUnit :: NonEmpty a -> Either a (NonUnit a)
nonUnit (a NE.:| as)
  | Just as' <- NE.nonEmpty as = Right (a :| as')
  | otherwise = Left a
