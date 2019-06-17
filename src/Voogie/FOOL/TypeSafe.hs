{-|
Module       : Voogie.FOOL.TypeSafe
Description  : Type-safe smart constructors of FOOL terms.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.FOOL.TypeSafe (
  store,
  select
) where

import Data.List.NonEmpty (NonEmpty((:|)))

import Voogie.FOOL
import Voogie.Pretty.Boogie.FOOL (pretty, displayS, renderCompact)

store :: Term -> Term -> Term -> Term
store arr index element
  | a@(Array (i :| _) _) <- typeOf arr
  , typeOf index == i
  , typeOf element == arrayArgument a = storeTerm
  | otherwise = error $ displayS (renderCompact $ pretty storeTerm)
                                 "ill-typed expression "
  where
    storeTerm = Store arr index element

select :: Term -> Term -> Term
select arr index
  | Array (i :| _) _ <- typeOf arr
  , typeOf index == i = selectTerm
  | otherwise = error $ displayS (renderCompact $ pretty selectTerm)
                                 "ill-typed expression "
  where
    selectTerm = Select arr index
