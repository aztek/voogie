{-# LANGUAGE PackageImports #-}

module Data.List.NonEmpty (
  module Data.List.NonEmpty
) where

import qualified Data.List as L (intercalate)
import "base" Data.List.NonEmpty
import qualified "base" Data.List.NonEmpty as NE (zipWith)

intercalate :: [a] -> NonEmpty [a] -> [a]
intercalate l = L.intercalate l . toList

zipWithM :: Applicative m
         => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithM f xs ys = sequenceA (NE.zipWith f xs ys)

one :: a -> NonEmpty a
one a = a :| []

two :: a -> a -> NonEmpty a
two a b = a :| [b]

three :: a -> a -> a -> NonEmpty a
three a b c = a :| [b, c]

four :: a -> a -> a -> a -> NonEmpty a
four a b c d = a :| [b, c, d]
