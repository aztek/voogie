{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Voogie.FOOL.Tuple where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

data Tuple a = a :| NonEmpty a
  deriving (Eq, Show, Ord, Functor, Foldable)

toList :: Tuple a -> [a]
toList (a :| ne) = a : NE.toList ne

toNonEmpty :: Tuple a -> NonEmpty a
toNonEmpty (a :| ne) = NE.cons a ne

nonUnit :: NonEmpty a -> Either a (Tuple a)
nonUnit (a NE.:| [])  = Left a
nonUnit (a NE.:| b:c) = Right (a :| (b NE.:| c))

intercalate :: String -> Tuple String -> String
intercalate s = L.intercalate s . toList
