{-# LANGUAGE DeriveFunctor #-}

module Kyckling.FOOL.Tuple where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

data Tuple a = a :| NonEmpty a
  deriving (Eq, Show, Functor)

toList :: Tuple a -> [a]
toList (a :| ne) = a : NE.toList ne

nonUnit :: NonEmpty a -> Either a (Tuple a)
nonUnit (a NE.:| [])  = Left a
nonUnit (a NE.:| b:c) = Right (a :| (b NE.:| c))