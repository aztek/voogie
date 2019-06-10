{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

{-|
Module       : Data.List.NonEmpty
Description  : Extra helper function for 'Data.List.NonEmpty' from "base".
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Data.List.NonEmpty (
  module Data.List.NonEmpty
) where

#if MIN_VERSION_base(4, 9, 0)
import "base" Data.List.NonEmpty
import qualified "base" Data.List.NonEmpty as NE (zipWith)
#else
import "semigroups" Data.List.NonEmpty
import qualified "semigroups" Data.List.NonEmpty as NE (zipWith)
#endif

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
