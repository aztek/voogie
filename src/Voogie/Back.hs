module Voogie.Back (translate, TranslationOptions(..)) where

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

import Voogie.TPTP

data TranslationOptions = TranslationOptions
  { useArrayTheory :: Bool
  }

updates :: B.Statement -> NonEmpty B.Var
updates = NE.nub . updates'
  where
    updates' (B.If _ _ a b) = sconcat $ fmap updates' (foldl (flip NE.cons) a b)
    updates' (B.Assign ass) = fmap (B.lvariable . fst) ass

translate :: TranslationOptions -> B.Boogie -> TPTP
translate opts (B.Boogie decls (B.Main _ pre stmts post))
  | not (useArrayTheory opts) = eliminateArrayTheory tptp
  | otherwise = tptp
  where
    tptp = TPTP [] decls pre (foldr nextState conjecture stmts)
    conjecture = F.conjunction post
    nextState = either translateStmt translateAssume

    translateStmt :: B.Statement -> F.Term -> F.Term
    translateStmt s = F.let_ (F.Binding (F.tupleD vars) (definition s))
      where
        vars = updates s

        definition :: B.Statement -> F.Term
        definition (B.Assign ass) = F.tupleLiteral (fmap translateAssign ass)
        definition (B.If c flipBranches a b)
          | flipBranches = F.if_ c' b' a'
          | otherwise    = F.if_ c' a' b'
          where
            c' = translateExpr c
            a' = foldr translateStmt tuple a
            b' = foldr translateStmt tuple b
            tuple = F.tupleLiteral (fmap F.constant vars)

    translateAssign :: B.Assignment -> F.Term
    translateAssign (lval, e)
      | Just is' <- is = F.store n is' e'
      | otherwise = e'
      where
        (n, is) = translateLValue lval
        e' = translateExpr e

    translateAssume :: B.Assume -> F.Term -> F.Term
    translateAssume (B.Assume f) = F.binary Imply f

    translateExpr :: B.Expression -> F.Term
    translateExpr = \case
      B.IntegerLiteral i -> F.integerConstant i
      B.BooleanLiteral b -> F.booleanConstant b
      B.Unary op e -> F.unary op (translateExpr e)
      B.Binary op a b -> F.binary op (translateExpr a) (translateExpr b)
      B.IfElse c a b -> F.if_ (translateExpr c) (translateExpr a) (translateExpr b)
      B.Equals s a b -> F.equals s (translateExpr a) (translateExpr b)
      B.FunApp f args -> F.application f (map translateExpr args)
      B.Ref lval -> maybe n (F.select n) is
        where
          (n, is) = translateLValue lval

    translateLValue :: B.LValue -> (F.Term, Maybe (NonEmpty F.Term))
    translateLValue (B.LValue n is) = (n', is')
      where
        n' = F.constant (F.name <$> n)
        is' = fmap translateExpr . sconcat <$> NE.nonEmpty is

eliminateArrayTheory :: TPTP -> TPTP
eliminateArrayTheory (TPTP types symbols axioms conjecture) =
  appendTheory tptp' (mconcat theories)
  where
    theories = map AT.theory (L.nub instantiations)

    Accumulate (tptp', instantiations) =
      TPTP types <$> traverse (rewriteSymbol typeRewriter) symbols
                 <*> traverse rewrite axioms
                 <*> rewrite conjecture

    rewrite = rewriteTerm termRewriter typeRewriter

    termRewriter :: Rewriter [AT.Instantiation] F.Term
    termRewriter (F.Select a i) = select <$> accumulateInstantiations (typeOf a)
      where
        select t = F.application . AT.selectSymbol <$> t <*> traverse rewrite [a, i]
    termRewriter (F.Store a i v) = store <$> accumulateInstantiations (typeOf a)
      where
        store t = F.application . AT.storeSymbol <$> t <*> traverse rewrite [a, i, v]
    termRewriter _ = Nothing

    typeRewriter :: Rewriter [AT.Instantiation] Type
    typeRewriter = fmap (fmap AT.arrayType) . accumulateInstantiations

    accumulateInstantiations :: Type -> Maybe (Accumulate [AT.Instantiation] AT.Instantiation)
    accumulateInstantiations (Array (t :| ts) s) = Just $ Accumulate (i, i:is)
      where
        Accumulate (i, is) = traverse (rewriteType typeRewriter) (t, array ts s)
    accumulateInstantiations _ = Nothing
