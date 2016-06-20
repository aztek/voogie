module Kyckling.Test.Program where

import Test.QuickCheck
import Test.QuickCheck.All

import Control.Monad
import Data.List

import Kyckling.Program

instance Arbitrary (Expr a) where
  arbitrary = frequency [ (1, liftM I arbitrary)
                        , (1, liftM B arbitrary)

                        , (1, liftM Not arbitrary)
                        , (1, liftM3 Conn arbitrary arbitrary arbitrary)
                        , (1, liftM3 Compare arbitrary arbitrary arbitrary)

                        , (1, liftM3 Op arbitrary arbitrary arbitrary)

                        , (1, liftM2 SelectI arbitrary arbitrary)
                        , (1, liftM2 SelectB arbitrary arbitrary)

                        , (1, liftM VarI  arbitrary)
                        , (1, liftM VarB  arbitrary)
                        , (1, liftM VarIA arbitrary)
                        , (1, liftM VarBA arbitrary) ]

instance Arbitrary Program where
  arbitrary = p
    where p = frequency [ (2, liftM3 IfElse arbitrary arbitrary arbitrary)
                        , (2, liftM2 If arbitrary arbitrary)
                        , (3, liftM2 (:=) arbitrary arbitrary)
                        , (4, liftM2 Compose arbitrary arbitrary) ]