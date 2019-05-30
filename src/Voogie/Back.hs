module Voogie.Back where

import Control.Monad.Writer

import qualified Data.List as L

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (sconcat)

import Voogie.Theory
import qualified Voogie.FOOL.Smart as F

import qualified Voogie.Boogie as B
import Voogie.Boogie.BoogiePretty()

import qualified Voogie.FOOL.ArrayTheory as AT
import Voogie.FOOL.Rewrite

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
  B.Assert f -> F.binary And f

translateExpr :: B.Expression -> F.Term
translateExpr = \case
  B.IntegerLiteral i -> F.integerConstant i
  B.BooleanLiteral b -> F.booleanConstant b
  B.Unary     op   e -> F.unary  op (translateExpr e)
  B.Binary    op a b -> F.binary op (translateExpr a) (translateExpr b)
  B.IfElse     c a b -> F.if_ (translateExpr c) (translateExpr a) (translateExpr b)
  B.Equals     s a b -> F.equals s (translateExpr a) (translateExpr b)
  B.FunApp    f args -> F.application f (fmap translateExpr args)
  B.Ref         lval -> maybe n (F.select n) is
    where
      (n, is) = translateLValue lval

translateLValue :: B.LValue -> (F.Term, Maybe (NonEmpty F.Term))
translateLValue (B.LValue n is) = (n', is')
  where
    n' = F.constant (F.name <$> n)
    is' = fmap translateExpr . sconcat <$> NE.nonEmpty is

eliminateArrayTheory :: F.Problem -> F.Problem
eliminateArrayTheory problem = F.appendTheory problem' theories
  where
    theories = mconcat . fmap AT.theory $ L.nub instantiations

    (problem', instantiations) =
      runWriter $ rewriteProblem termRewriter typeRewriter problem

    termRewriter :: Rewriter [AT.Instantiation] F.Term
    termRewriter = \case
      F.Select a i  -> application AT.selectSymbol a [i]
      F.Store a i v -> application AT.storeSymbol  a [i, v]
      _ -> Nothing

    application s f as = app <$> accumulateInstantiations (typeOf f)
      where
        app t = F.application . s <$> t <*> traverse (rewriteTerm termRewriter typeRewriter) (f :| as)

    typeRewriter :: Rewriter [AT.Instantiation] Type
    typeRewriter = fmap (fmap AT.arrayType) . accumulateInstantiations

    accumulateInstantiations :: Type -> Maybe (Writer [AT.Instantiation] AT.Instantiation)
    accumulateInstantiations = \case
      Array (t :| ts) s -> Just . mapWriter (\(i, is) -> (i, i:is))
                         $ traverse (rewriteType typeRewriter) (t, array ts s)
      _ -> Nothing
