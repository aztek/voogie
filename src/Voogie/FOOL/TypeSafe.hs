module Voogie.FOOL.TypeSafe (
  module Voogie.FOOL.TypeSafe,
  Var, Identifier, Definition(..), Binding(..), Term, Formula
) where

import Data.List.NonEmpty (NonEmpty((:|)))

import Voogie.Theory

import Voogie.FOOL
import Voogie.FOOL.Pretty

typeSafeStore :: Term -> Term -> Term -> Term
typeSafeStore array index element =
  case (typeOf array, typeOf index, typeOf element) of
       (a@(Array (i :| _) _), i', e) | i == i' && e == arrayArgument a -> store
       _ -> error $ "ill-typed expression " ++ pretty store
  where
    store = Store array index element

typeSafeSelect :: Term -> Term -> Term
typeSafeSelect array index =
  case (typeOf array, typeOf index) of 
       (Array (i :| _) _, i') | i == i' -> select
       _ -> error $ "ill-typed expression " ++ pretty select
  where
    select = Select array index