module Kyckling.Pretty where

class Pretty a where
  pretty :: a -> String