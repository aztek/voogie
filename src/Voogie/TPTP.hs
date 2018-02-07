module Voogie.TPTP where

import Voogie.Theory
import Voogie.FOOL

data TPTP = TPTP
  { types :: [Name]
  , symbols :: [Identifier]
  , axioms :: [Formula]
  , conjecture :: Formula
  }
  deriving (Show, Eq)
