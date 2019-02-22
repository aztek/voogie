{-# LANGUAGE TemplateHaskell #-}

module Back where

import Test.QuickCheck

import Voogie.Boogie
import Voogie.Theory

import Generators ()

import Voogie.Back

prop_translateExprPreservesTyping :: Expression -> Bool
prop_translateExprPreservesTyping e = typeOf e == typeOf (translateExpr e)

return []
main = $forAllProperties $ quickCheckWithResult stdArgs{maxSuccess=100000}
