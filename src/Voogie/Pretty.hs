module Voogie.Pretty where

class Pretty a where
  indented :: Integer -> a -> String
  indented _ = pretty

  pretty :: a -> String
  pretty = indented 0

parens :: String -> String
parens s = "(" ++ s ++ ")"

brackets :: String -> String
brackets s = "[" ++ s ++ "]"