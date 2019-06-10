{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module       : Generators
Description  : QuickCheck generators of Boogie expressions.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Generators (
  main
) where

import Control.Monad ((<=<))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as NE (NonEmpty((:|)))
import Data.List.NonUnit (NonUnit)
import qualified Data.List.NonUnit as NU (NonUnit((:|)))

import qualified Test.QuickCheck as QC (Property)
import Test.QuickCheck (Arbitrary(..), Gen, Positive(..), Args(..),
                        elements, choose, oneof, sized, suchThat, forAll,
                        shrinkList, stdArgs,
                        forAllProperties, quickCheckWithResult)

import Voogie.Boogie hiding (array, tuple)

instance Arbitrary Quantifier where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary UnaryOp where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary BinaryOp where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Sign where
  arbitrary = elements [minBound..maxBound]

arbitraryName :: Gen Name
arbitraryName = elements $ fmap (:[]) ['a'..'z']

split :: Int -> Gen (Int, Int)
split n = do
  k <- choose (0, n)
  m <- choose (0, n - k)
  return (k, m)

prop_split :: Positive Int -> QC.Property
prop_split (Positive n) = forAll (split n) $ \(k, m) -> k + m <= n

splitList :: Int -> Gen [Int]
splitList = \case
  0 -> return []
  n -> do
    (k, m) <- split n
    ks <- splitList m
    return (k : ks)

prop_splitList :: Positive Int -> QC.Property
prop_splitList (Positive n) = forAll (splitList n) $ \ks -> sum ks <= n

splitNE :: Int -> Gen (NonEmpty Int)
splitNE n = do
  (k, m) <- split n
  ks <- splitList m
  return (k NE.:| ks)

prop_splitNE :: Positive Int -> QC.Property
prop_splitNE (Positive n) = forAll (splitNE n) $ \ks -> sum ks <= n

splitNU :: Int -> Gen (NonUnit Int)
splitNU n = do
  (k, m) <- split n
  ks <- splitNE m
  return (k NU.:| ks)

prop_splitNU :: Positive Int -> QC.Property
prop_splitNU (Positive n) = forAll (splitNU n) $ \ks -> sum ks <= n

instance Arbitrary Type where
  -- Currently, truly arbitrary types significantly slow down generation of
  -- well-typed expressions. Perhaps, a different approach is needed.
  -- TODO: generate non-atomic types as well
  arbitrary = sizedType 0

  shrink = \case
    Array      is r -> r : toList is
    Tuple        ts -> toList ts
    Functional as r -> r : toList as
    _ -> []

sizedType :: Int -> Gen Type
sizedType n = oneof
            $ atomic
           ++ if n < 1 then []
              else [array, tuple, functional]
  where
    atomic = return <$> [Integer, Boolean]

    array = do
      (r NU.:| is) <- traverse sizedType =<< splitNU (n - 1)
      return (Array is r)

    tuple = do
      ts <- traverse sizedType =<< splitNU (n - 1)
      return (Tuple ts)

    functional = do
      (r NU.:| as) <- traverse sizedType =<< splitNU (n - 1)
      return (Functional as r)

    -- custom = do
    --   n <- arbitraryName
    --   return (Custom n)

sizedLValue :: Int -> Gen LValue
sizedLValue n = do
  ns <- splitList n
  is <- traverse (traverse sizedExpression <=< splitNE) ns
  r <- arbitrary
  v <- arbitraryName
  let t = foldr Array r (fmap (fmap typeOf) is)
  return $ LValue (Typed t v) is

instance Arbitrary LValue where
  arbitrary = sized sizedLValue

typed :: Type -> Expression -> Bool
typed t e = typeOf e == t

sizedExpression :: Int -> Gen Expression
sizedExpression n = oneof
                  $ [integerLiteral, booleanLiteral]
                 ++ if n < 1 then []
                    else [ref, unary, binary, ifElse, {-funApp,-} equals]
  where
    integerLiteral = IntegerLiteral <$> arbitrary
    booleanLiteral = BooleanLiteral <$> arbitrary

    ref = do
      k <- choose (0, n - 1)
      lval <- sizedLValue k
      return (Ref lval)

    unary = do
      op <- arbitrary
      let t = unaryOpDomain op
      (k, _) <- split (n - 1)
      e <- sizedExpression k `suchThat` typed t
      return (Unary op e)

    binary = do
      op <- arbitrary
      let (t1, t2) = binaryOpDomain op
      (as, bs) <- split (n - 1)
      a <- sizedExpression as `suchThat` typed t1
      b <- sizedExpression bs `suchThat` typed t2
      return (Binary op a b)

    ifElse = do
      (cs, s) <- split (n - 1)
      (as, bs) <- split s
      c <- sizedExpression cs `suchThat` typed Boolean
      a <- sizedExpression as
      b <- sizedExpression bs `suchThat` typed (typeOf a)
      return (IfElse c a b)

    -- funApp = do
    --   es <- traverse sizedExpression =<< splitNE (n - 1)
    --   f <- arbitraryName
    --   let ts = fmap typeOf es
    --   r <- arbitrary
    --   return (FunApp (Typed (Functional ts r) f) es)

    equals = do
      s <- arbitrary
      (as, bs) <- split (n - 1)
      a <- sizedExpression as
      b <- sizedExpression bs `suchThat` typed (typeOf a)
      return (Equals s a b)

instance Arbitrary Expression where
  arbitrary = sized sizedExpression

  shrink = \case
    Unary    op e -> e : (Unary op <$> shrink e)
    Binary op a b -> a : b : (Binary op <$> shrink a <*> shrink b)
    IfElse  c a b -> a : b : c : (IfElse <$> shrink c <*> shrink a <*> shrink b)
    Equals  s a b -> a : b : (Equals s <$> shrink a <*> shrink b)
    _ -> []

sizedAssignment :: Int -> Gen Assignment
sizedAssignment n = do
  (ls, rs) <- split (n - 1)
  lval <- sizedLValue ls
  rval <- sizedExpression rs `suchThat` typed (typeOf lval)
  return (lval, rval)

sizedStatement :: Int -> Gen Statement
sizedStatement n = oneof $ assign : [if_ | n > 1]
  where
    assign = do
      as <- traverse sizedAssignment =<< splitNE 5
      return (Assign as)

    if_ = do
      c <- sizedExpression 4 `suchThat` typed Boolean
      f <- arbitrary
      (as, bs) <- split n
      as' <- traverse sizedStatement =<< splitNE as
      bs' <- traverse sizedStatement =<< splitList bs
      return (If c f as' bs')

instance Arbitrary Statement where
  arbitrary = sized sizedStatement

  shrink = \case
    If c f as bs -> toList as ++ bs ++ (If c f <$> traverse shrink as <*> shrinkList shrink bs)
    _ -> []

return []

main :: IO Bool
main = $forAllProperties $ quickCheckWithResult stdArgs{maxSuccess=1000}
