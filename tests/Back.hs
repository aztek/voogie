{-# LANGUAGE TemplateHaskell #-}

module Back (
  main
) where

import Test.QuickCheck (Property, Args(..), (===),
                        stdArgs, forAllProperties, quickCheckWithResult)

import Generators ()

import Voogie.Back (translateExpr)
import Voogie.Boogie (Expression)
import Voogie.Language (typeOf)

prop_translateExprPreservesTyping :: Expression -> Property
prop_translateExprPreservesTyping e = typeOf e === typeOf (translateExpr e)

return []

main :: IO Bool
main = $forAllProperties $ quickCheckWithResult stdArgs{maxSuccess=100000}
