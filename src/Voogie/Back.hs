{-# LANGUAGE LambdaCase #-}

{-|
Module       : Voogie.Back
Description  : Translator of property-annotated Boogie programs to FOOL problems.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.Back (
  TranslationOptions(..),
  translate,
  translateBoogie,
  translateStatement,
  translateAssign,
  translateProperty,
  translateExpr,
  translateLValue
) where

import Control.Monad.Writer (Writer, runWriter, tell)
import qualified Data.List as L (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE (nub, cons)
import Data.Semigroup (sconcat)

import qualified Voogie.Boogie as B
import qualified Voogie.FOOL.ArrayTheory as AT
import qualified Voogie.FOOL.Smart as F
import Voogie.FOOL.Traverse
import Voogie.Language hiding (tuple)
import Voogie.Pretty.Boogie.Boogie ()

data TranslationOptions = TranslationOptions {
  useArrayTheory :: Bool
} deriving (Show, Eq, Ord)

updates :: B.Statement -> NonEmpty B.Var
updates = NE.nub . updates'
  where
    updates' (B.If _ _ a b) = sconcat (updates' <$> foldr NE.cons a b)
    updates' (B.Assign ass) = B.lvariable . fst <$> ass

translate :: TranslationOptions -> B.Boogie -> F.Problem
translate to boogie
  | not (useArrayTheory to) = eliminateArrayTheory (translateBoogie boogie)
  | otherwise = translateBoogie boogie

translateBoogie :: B.Boogie -> F.Problem
translateBoogie (B.Boogie decls (B.Main _ pre stmts post)) = problem
  where
    problem = F.Problem [] decls pre (foldr nextState conjecture stmts)
    conjecture = F.conjunction post
    nextState = either translateStatement translateProperty

translateStatement :: B.Statement -> F.Term -> F.Term
translateStatement s = F.let_ (F.Binding definition body)
  where
    vars = updates s

    definition = F.tupleD vars

    body = case s of
      B.Assign ass -> F.tupleLiteral (translateAssign <$> ass)
      B.If c flipBranches a b
        | flipBranches -> F.ifElse c' b' a'
        | otherwise    -> F.ifElse c' a' b'
        where
          c' = translateExpr c
          a' = foldr translateStatement tuple a
          b' = foldr translateStatement tuple b
          tuple = F.tupleLiteral (F.constant <$> vars)

translateAssign :: B.Assignment -> F.Term
translateAssign (lval, e) = F.foldStore F.select F.store n is (translateExpr e)
  where (n, is) = translateLValue lval

translateProperty :: B.Property -> F.Term -> F.Term
translateProperty = \case
  B.Assume f -> F.binary Imply f
  B.Assert f -> F.binary And   f

translateExpr :: B.Expression -> F.Term
translateExpr = \case
  B.IntegerLiteral i -> F.integerConstant i
  B.BooleanLiteral b -> F.booleanConstant b
  B.Unary     op   e -> F.unary  op (translateExpr e)
  B.Binary    op a b -> F.binary op (translateExpr a) (translateExpr b)
  B.IfElse     c a b -> F.ifElse (translateExpr c) (translateExpr a) (translateExpr b)
  B.Equals     s a b -> F.equals s (translateExpr a) (translateExpr b)
  B.Application f es -> F.application f (fmap translateExpr es)
  B.Ref         lval -> uncurry (foldl F.select) (translateLValue lval)

translateLValue :: B.LValue -> (F.Term, [NonEmpty F.Term])
translateLValue (B.LValue n is) = (F.constant (F.name <$> n), fmap translateExpr <$> is)

eliminateArrayTheory :: F.Problem -> F.Problem
eliminateArrayTheory problem = F.appendTheories problem' theories
  where
    theories = fmap AT.theory (L.nub instantiations)
    (problem', instantiations) = runWriter (eliminateProblem problem)

    eliminateProblem :: F.Problem -> Writer [AT.Instantiation] F.Problem
    eliminateProblem = traverseProblem eliminateTerm eliminateType

    eliminateTerm :: F.Term -> Writer [AT.Instantiation] F.Term
    eliminateTerm = \case
      F.Select a i   -> select a i
      F.Store  a i v -> store  a i v
      t -> traverseTerm eliminateTerm eliminateType t

    select :: F.Term -> NonEmpty F.Term -> Writer [AT.Instantiation] F.Term
    select a i =  AT.select
              <$> instantiation (fmap typeOf i) (arrayArgument (typeOf a))
              <*> eliminateTerm a
              <*> traverse eliminateTerm i

    store :: F.Term -> NonEmpty F.Term -> F.Term -> Writer [AT.Instantiation] F.Term
    store a i v =  AT.store
               <$> instantiation (fmap typeOf i) (typeOf v)
               <*> eliminateTerm a
               <*> traverse eliminateTerm i
               <*> eliminateTerm v

    eliminateType :: Type -> Writer [AT.Instantiation] Type
    eliminateType = \case
      Array ts r -> do
        inst <- instantiation ts r
        tell [inst]
        return (AT.arrayType inst)
      t -> traverseType eliminateType t

    instantiation :: NonEmpty Type -> Type -> Writer [AT.Instantiation] AT.Instantiation
    instantiation = curry (traverse eliminateType)
