{-# LANGUAGE GADTs #-}
module Voogie.Error where

import Voogie.Theory
import Voogie.BoogiePretty
import Text.Parsec

type Result = Either Error

data Error where
  ParsingError :: ParseError -> Error
  UndefinedVariable :: Name -> Error
  MultipleDefinitions :: Name -> Error
  TypeMismatch :: BoogiePretty a => SourcePos -> Type -> Type -> a -> Error
  NonArraySelect :: BoogiePretty a => Type -> a -> Error
  ArrayDimensionMismatch :: Type -> Error

renderError :: Error -> String
renderError e = case e of
  ParsingError e -> "parsing error: " ++ show e
  UndefinedVariable n -> "undefined variable " ++ n
  MultipleDefinitions n -> "redefined variable " ++ n
  TypeMismatch pos t t' a ->
    show pos ++ ": Type error: expected an expression of the type " ++
    pretty t ++ " but got " ++ pretty a ++ " of the type " ++ pretty t'
  NonArraySelect t a -> "expected an expression of an array type," ++
                        " but got " ++ pretty a ++ " of the type " ++ pretty t
  ArrayDimensionMismatch t -> "Too many array indexes for type " ++ show t
