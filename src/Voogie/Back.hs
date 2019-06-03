{-# LANGUAGE LambdaCase #-}

module Voogie.Back where

import Control.Monad.Writer (Writer, runWriter, tell)
import qualified Data.List as L (nub)
import qualified Data.List.NonEmpty as NE (nub, cons, nonEmpty)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Voogie.NonEmpty as VNE (two, three)
import Data.Semigroup (sconcat)

import Voogie.Theory
import qualified Voogie.Boogie as B
import Voogie.Boogie.BoogiePretty()
import qualified Voogie.FOOL.ArrayTheory as AT
import qualified Voogie.FOOL.Smart as F
import Voogie.FOOL.Traverse

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
        | flipBranches -> F.if_ c' b' a'
        | otherwise    -> F.if_ c' a' b'
        where
          c' = translateExpr c
          a' = foldr translateStatement tuple a
          b' = foldr translateStatement tuple b
          tuple = F.tupleLiteral (F.constant <$> vars)

translateAssign :: B.Assignment -> F.Term
translateAssign (lval, e) = maybe id (F.store n) is (translateExpr e)
  where
    (n, is) = translateLValue lval

translateProperty :: B.Property -> F.Term -> F.Term
translateProperty = \case
  B.Assume f -> F.binary Imply f
  B.Assert f -> F.binary And  f

translateExpr :: B.Expression -> F.Term
translateExpr = \case
  B.IntegerLiteral i -> F.integerConstant i
  B.BooleanLiteral b -> F.booleanConstant b
  B.Unary  op   e -> F.unary  op (translateExpr e)
  B.Binary op a b -> F.binary op (translateExpr a) (translateExpr b)
  B.IfElse  c a b -> F.if_ (translateExpr c) (translateExpr a) (translateExpr b)
  B.Equals  s a b -> F.equals s (translateExpr a) (translateExpr b)
  B.FunApp f args -> F.application f (fmap translateExpr args)
  B.Ref      lval -> maybe n (F.select n) is
    where
      (n, is) = translateLValue lval

translateLValue :: B.LValue -> (F.Term, Maybe (NonEmpty F.Term))
translateLValue (B.LValue n is) = (n', is')
  where
    n' = F.constant (F.name <$> n)
    is' = fmap translateExpr . sconcat <$> NE.nonEmpty is

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

    select :: F.Term -> F.Term -> Writer [AT.Instantiation] F.Term
    select a i =  F.application
              <$> (AT.selectSymbol <$> inst)
              <*> traverse eliminateTerm (VNE.two a i)
      where
        inst = instantiation (typeOf i) (arrayArgument (typeOf a))

    store :: F.Term -> F.Term -> F.Term -> Writer [AT.Instantiation] F.Term
    store a i v =  F.application
               <$> (AT.storeSymbol <$> inst)
               <*> traverse eliminateTerm (VNE.three a i v)
      where
        inst = instantiation (typeOf i) (typeOf v)

    eliminateType :: Type -> Writer [AT.Instantiation] Type
    eliminateType = \case
      Array (t :| ts) r -> do
        inst <- instantiation t (array ts r)
        tell [inst]
        return (AT.arrayType inst)
      t -> traverseType eliminateType t

    instantiation :: Type -> Type -> Writer [AT.Instantiation] AT.Instantiation
    instantiation = curry (traverse eliminateType)
