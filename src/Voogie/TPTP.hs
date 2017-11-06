module Voogie.TPTP where

import Voogie.Theory
import Voogie.FOOL

data TPTP =
  TPTP { signature :: [Typed Name]
       , axioms :: [Formula]
       , conjecture :: Formula
       }
  deriving (Show, Eq)
