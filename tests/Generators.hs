{-# LANGUAGE TemplateHaskell #-}

module Generators where

import Test.QuickCheck

import Data.Maybe
import Data.List as L
import Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Control.Monad

import qualified Voogie.FOOL.Tuple as Tuple
import Voogie.FOOL.Tuple (Tuple)

import Voogie.Theory
import Voogie.Boogie

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

prop_split :: Positive Int -> Property
prop_split (Positive n) = forAll (split n) $ \(k, m) -> k + m <= n

splitList :: Int -> Gen [Int]
splitList = \case
  0 -> return []
  n -> do
    (k, m) <- split n
    ks <- splitList m
    return (k : ks)

prop_splitList :: Positive Int -> Property
prop_splitList (Positive n) = forAll (splitList n) $ \ks -> sum ks <= n

splitNE :: Int -> Gen (NonEmpty Int)
splitNE n = do
  (k, m) <- split n
  ks <- splitList m
  return (k NE.:| ks)

prop_splitNE :: Positive Int -> Property
prop_splitNE (Positive n) = forAll (splitNE n) $ \ks -> sum (NE.toList ks) <= n

splitTuple :: Int -> Gen (Tuple Int)
splitTuple n = do
  (k, m) <- split n
  ks <- splitNE m
  return (k Tuple.:| ks)

prop_splitTuple :: Positive Int -> Property
prop_splitTuple (Positive n) = forAll (splitTuple n) $ \ks -> sum (Tuple.toList ks) <= n

instance Arbitrary Type where
  -- Currently, truly arbitrary types significantly slow down generation of
  -- well-typed expressions. Perhaps, a different approach is needed.
  -- TODO: generate non-atomic types as well
  arbitrary = sizedType 0

  shrink = \case
    Array is r -> r : NE.toList is
    TupleType ts -> Tuple.toList ts
    Functional as r -> r : NE.toList as
    _ -> []

sizedType :: Int -> Gen Type
sizedType n = oneof
            $ atomic
           ++ if n < 1 then []
              else [array, tupleType, functional]
  where
    atomic = return <$> [Integer, Boolean]

    array = do
      (r Tuple.:| is) <- traverse sizedType =<< splitTuple (n - 1)
      return (Array is r)

    tupleType = do
      ts <- traverse sizedType =<< splitTuple (n - 1)
      return (TupleType ts)

    functional = do
      (r Tuple.:| as) <- traverse sizedType =<< splitTuple (n - 1)
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
                  $ [intLiteral, boolLiteral]
                 ++ if n < 1 then []
                    else [ref, unary, binary, ifElse, {-funApp,-} equals]
  where
    intLiteral = IntegerLiteral <$> arbitrary
    boolLiteral = BooleanLiteral <$> arbitrary

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
    Unary _ e -> [e]
    Binary _ a b -> [a, b]
    IfElse c a b -> [a, b, c]
    Equals _ a b -> [a, b]
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
      as <- traverse sizedStatement =<< splitNE as
      bs <- traverse sizedStatement =<< splitList bs
      return (If c f as bs)

instance Arbitrary Statement where
  arbitrary = sized sizedStatement

  shrink = \case
    If _ _ as bs -> NE.toList as ++ bs
    _ -> []

return []
main = $forAllProperties $ quickCheckWithResult stdArgs{maxSuccess=1000}
