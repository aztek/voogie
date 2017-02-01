module Voogie.TPTP where

import Data.List
import qualified Voogie.FOOL.Tuple as Tuple
import Voogie.FOOL.Tuple (Tuple)
import Data.Char

import Voogie.Theory
import Voogie.FOOL

data TPTP = TPTP [Unit]
  deriving (Show, Eq)

data Unit = Type (Typed Name)
          | Axiom Formula
          | Conjecture Formula
  deriving (Show, Eq)