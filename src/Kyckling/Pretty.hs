module Kyckling.Pretty where

class Pretty a where
  indented :: Integer -> a -> String
  indented _ = pretty

  pretty :: a -> String
  pretty = indented 0