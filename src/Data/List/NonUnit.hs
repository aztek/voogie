{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.List.NonUnit where

import qualified Data.List as L (intercalate)
import qualified Data.List.NonEmpty as NE (toList, cons, nonEmpty, NonEmpty(..))
import Data.List.NonEmpty (NonEmpty)

data NonUnit a = a :| NonEmpty a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

toList :: NonUnit a -> [a]
toList (a :| ne) = a : NE.toList ne

toNonEmpty :: NonUnit a -> NonEmpty a
toNonEmpty (a :| ne) = NE.cons a ne

nonUnit :: NonEmpty a -> Either a (NonUnit a)
nonUnit (a NE.:| as)
  | Just as' <- NE.nonEmpty as = Right (a :| as')
  | otherwise = Left a

intercalate :: [a] -> NonUnit [a] -> [a]
intercalate l = L.intercalate l . toList
