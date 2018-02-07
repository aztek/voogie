{-# LANGUAGE PatternGuards #-}

module Voogie.FOOL.TypeSafe (
  module Voogie.FOOL.TypeSafe,
  Var, Identifier, Definition(..), Binding(..), Term, Formula
) where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

import Voogie.Theory

import Voogie.FOOL
import Voogie.FOOL.Pretty

typeSafeApplication :: Identifier -> NonEmpty Term -> Term
typeSafeApplication f ts
  | Functional as _ <- typeOf f
  , length as == length ts
  , and $ NE.zipWith (\t a -> typeOf t == a) ts as = application
  | otherwise = error ("ill-typed expression " ++ pretty application)
  where
    application = Application f ts

typeSafeStore :: Term -> Term -> Term -> Term
typeSafeStore array index element
  | a@(Array (i :| _) _) <- typeOf array
  , typeOf index == i
  , typeOf element == arrayArgument a = store
  | otherwise = error ("ill-typed expression " ++ pretty store)
  where
    store = Store array index element

typeSafeSelect :: Term -> Term -> Term
typeSafeSelect array index
  | Array (i :| _) _ <- typeOf array
  , typeOf index == i = select
  | otherwise = error ("ill-typed expression " ++ pretty select)
  where
    select = Select array index
