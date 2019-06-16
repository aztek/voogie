{-|
Module       : Voogie.FOOL.TypeSafe
Description  : Type-safe smart constructors of FOOL terms.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.FOOL.TypeSafe (
  Var,
  Identifier,
  Definition(..),
  Binding(..),
  Term,
  Formula,
  typeSafeStore,
  typeSafeSelect
) where

import Data.List.NonEmpty (NonEmpty((:|)))

import Voogie.FOOL
import Voogie.Pretty.Boogie.FOOL (pretty, displayS, renderCompact)

typeSafeStore :: Term -> Term -> Term -> Term
typeSafeStore arr index element
  | a@(Array (i :| _) _) <- typeOf arr
  , typeOf index == i
  , typeOf element == arrayArgument a = store
  | otherwise = error $ displayS (renderCompact $ pretty store)
                                 "ill-typed expression "
  where
    store = Store arr index element

typeSafeSelect :: Term -> Term -> Term
typeSafeSelect arr index
  | Array (i :| _) _ <- typeOf arr
  , typeOf index == i = select
  | otherwise = error $ displayS (renderCompact $ pretty select)
                                 "ill-typed expression "
  where
    select = Select arr index
