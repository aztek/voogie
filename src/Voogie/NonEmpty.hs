module Voogie.NonEmpty where

import qualified Data.List as L

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

intercalate :: String -> NonEmpty String -> String
intercalate s = L.intercalate s . NE.toList

one :: a -> NonEmpty a
one a = a NE.:| []

two :: a -> a -> NonEmpty a
two a b = a NE.:| [b]

three :: a -> a -> a -> NonEmpty a
three a b c = a NE.:| [b, c]
