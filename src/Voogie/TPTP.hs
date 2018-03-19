module Voogie.TPTP (
  TPTP(..), appendTheory
) where

import Data.Semigroup ((<>))

import Voogie.Theory
import Voogie.FOOL

data TPTP = TPTP
  { types :: [Name]
  , symbols :: [Identifier]
  , axioms :: [Formula]
  , conjecture :: Formula
  }
  deriving (Show, Eq)

appendTheory :: TPTP -> Theory -> TPTP
appendTheory (TPTP ts ss as c) th = TPTP ts' ss' as' c
  where Theory ts' ss' as' = th <> Theory ts ss as
