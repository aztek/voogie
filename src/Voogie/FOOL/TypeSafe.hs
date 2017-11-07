{-# LANGUAGE PatternGuards #-}

module Voogie.FOOL.TypeSafe (
  module Voogie.FOOL.TypeSafe,
  Var, Identifier, Definition(..), Binding(..), Term, Formula
) where

import Data.List.NonEmpty (NonEmpty((:|)))

import Voogie.Theory

import Voogie.FOOL
import Voogie.FOOL.Pretty

typeSafeStore :: Term -> Term -> Term -> Term
typeSafeStore array index element
  | a@(Array (i :| _) _) <- typeOf array
  , i == typeOf index, typeOf element == arrayArgument a = store
  | otherwise = error ("ill-typed expression " ++ pretty store)
  where
    store = Store array index element

typeSafeSelect :: Term -> Term -> Term
typeSafeSelect array index
  | Array (i :| _) _ <- typeOf array, i == typeOf index = select
  | otherwise = error ("ill-typed expression " ++ pretty select)
  where
    select = Select array index
