{-# LANGUAGE GADTs #-}
module Voogie.Error (
  Result, Error(..), renderError
) where

import Data.List

import Voogie.Theory
import Voogie.AST
import Voogie.BoogiePretty

import Text.Parsec
import Text.Parsec.Error

type Result = Either Error

data Error where
  ParsingError :: ParseError -> Error
  UndefinedVariable :: AST Name -> Error
  MultipleDefinitions :: AST (Typed Name) -> Error
  TypeMismatch :: BoogiePretty a => AST (Typed a) -> Type -> Error
  NonArraySelect :: BoogiePretty a => AST (Typed a) -> Error
  ArrayDimensionMismatch :: BoogiePretty a => AST (Typed a) -> Error

renderError :: String -> Error -> String
renderError contents error = case error of
  ParsingError err -> renderError' (errorPos err)
    ["failed to parse",
     showErrorMessages "or" "unknown parse error" "expecting" "unexpected"
                       "end of input" (errorMessages err)]
  UndefinedVariable (AST pos v) -> renderError' pos
    ["variable not in scope:", v]
  MultipleDefinitions (AST pos v) -> renderError' pos
    ["variable redefined:", renderTyped v]
  TypeMismatch (AST pos a) t' -> renderError' pos
    ["expected an expression of the type", pretty t', "but got", renderTyped a]
  NonArraySelect (AST pos a) -> renderError' pos
    ["expected an expression of an array type,", "but got", renderTyped a]
  ArrayDimensionMismatch (AST pos a) -> renderError' pos
    ["too many array indexes for", renderTyped a]
  where
    renderError' :: SourcePos -> [String] -> String
    renderError' pos msg = intercalate "\n" [
      unwords (renderPosition pos : "error:" : msg),
      "",
      lines contents !! (sourceLine pos - 1),
      replicate (sourceColumn pos - 1) ' ' ++ "^"
      ]

renderTyped :: BoogiePretty a => Typed a -> String
renderTyped (Typed t a) = unwords [pretty a, "of the type", pretty t]

renderPosition :: SourcePos -> String
renderPosition pos = intercalate ":" [source, line, column] ++ ":"
  where
    source = sourceName pos
    line = show (sourceLine pos)
    column = show (sourceColumn pos)
