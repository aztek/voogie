{-# LANGUAGE TemplateHaskell #-}

module Back where

import Test.QuickCheck

import Generators ()

import Voogie.Back (translateExpr)
import Voogie.Boogie (Expression)
import Voogie.Theory (typeOf)

prop_translateExprPreservesTyping :: Expression -> Property
prop_translateExprPreservesTyping e = typeOf e === typeOf (translateExpr e)

return []

main :: IO Bool
main = $forAllProperties $ quickCheckWithResult stdArgs{maxSuccess=100000}
