module Voogie.NonEmpty where

import qualified Data.List as L

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

intercalate :: String -> NonEmpty String -> String
intercalate s = L.intercalate s . NE.toList

zipWithM :: Applicative m => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithM f xs ys = sequenceA (NE.zipWith f xs ys)

one :: a -> NonEmpty a
one a = a NE.:| []

two :: a -> a -> NonEmpty a
two a b = a NE.:| [b]

three :: a -> a -> a -> NonEmpty a
three a b c = a NE.:| [b, c]
