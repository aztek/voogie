module Voogie.TPTP where

import Voogie.Theory
import Voogie.FOOL

data TPTP = TPTP [Unit]
  deriving (Show, Eq)

data Unit = Type (Typed Name)
          | Axiom Formula
          | Conjecture Formula
  deriving (Show, Eq)