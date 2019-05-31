module Voogie.NonEmpty where

import qualified Data.List as L (intercalate)

import qualified Data.List.NonEmpty as NE (toList, zipWith)
import Data.List.NonEmpty (NonEmpty((:|)))

intercalate :: [a] -> NonEmpty [a] -> [a]
intercalate l = L.intercalate l . NE.toList

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
