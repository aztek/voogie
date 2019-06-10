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
  typeSafeApplication,
  typeSafeStore,
  typeSafeSelect
) where

import qualified Data.List.NonEmpty as NE (zipWith)
import Data.List.NonEmpty (NonEmpty((:|)))

import Voogie.FOOL
import Voogie.Pretty.Boogie.FOOL (pretty, displayS, renderCompact)

typeSafeApplication :: Identifier -> NonEmpty Term -> Term
typeSafeApplication f ts
  | Functional as _ <- typeOf f
  , length as == length ts
  , and $ NE.zipWith (\t a -> typeOf t == a) ts as = application
  | otherwise = error $ displayS (renderCompact $ pretty application)
                                 "ill-typed expression "
  where
    application = Application f ts

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
