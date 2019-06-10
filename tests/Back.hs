{-# LANGUAGE TemplateHaskell #-}

{-|
Module       : Back
Description  : QuickCheck properties of the voogie translator.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

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
