{-# LANGUAGE PatternGuards #-}

module Voogie.Back (
  translate, TranslationOptions(..)
) where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)

import Voogie.Theory
import qualified Voogie.FOOL.Smart as F

import qualified Voogie.Boogie as B
import Voogie.Boogie.Pretty()

import Voogie.TPTP

data TranslationOptions =
  TranslationOptions { useArrayTheory :: Bool
                     }

updates :: B.Statement -> NonEmpty B.Var
updates = NE.nub . updates'
  where
    updates' (B.If _ _ a b) = sconcat $ fmap updates' (foldl (flip NE.cons) a b)
    updates' (B.Assign ass) = fmap (B.lvariable . fst) ass

translate :: TranslationOptions -> B.Boogie -> TPTP
translate _opts (B.Boogie decls (B.Main pre stmts post))
  | Just post <- NE.nonEmpty post = TPTP signature axioms (conjecture post)
  | otherwise = TPTP [] [] (F.booleanConstant True)
  where
    signature = decls
    axioms    = pre
    conjecture post = foldr (either translateStatement translateAssume)
                            (foldr1 (F.binary And) post) stmts

    translateStatement :: B.Statement -> F.Term -> F.Term
    translateStatement s = F.let_ (F.Binding (F.tupleD vars) (body s))
      where
        vars = updates s

        body :: B.Statement -> F.Term
        body (B.Assign ass) = F.tupleLiteral (fmap analyzeAssignment vars)
          where
            assignments = NE.zip (fmap (B.lvariable . fst) ass) ass
            analyzeAssignment :: B.Var -> F.Term
            analyzeAssignment v = maybe (F.constant v) translateAssign maybeAssign
              where maybeAssign = lookup v (NE.toList assignments)
        body (B.If c f a b) = if f then F.if_ c' b' a' else F.if_ c' a' b'
          where
            tuple = F.tupleLiteral (fmap F.constant vars)
            a' = foldr translateStatement tuple a
            b' = foldr translateStatement tuple b
            c' = translateExpression c

    translateAssume :: B.Assume -> F.Term -> F.Term
    translateAssume (B.Assume f) = F.binary Imply f

    translateAssign :: (B.LValue, B.Expression) -> F.Term
    translateAssign (lval, e) = maybe e' (\ is' -> F.store n is' e') is
      where
        (n, is) = translateLValue lval
        e' = translateExpression e

    translateExpression :: B.Expression -> F.Term
    translateExpression (B.IntegerLiteral i) = F.integerConstant i
    translateExpression (B.BooleanLiteral b) = F.booleanConstant b
    translateExpression (B.Unary  op e)   = F.unary  op (translateExpression e)
    translateExpression (B.Binary op a b) = F.binary op (translateExpression a) (translateExpression b)
    translateExpression (B.IfElse c a b)  = F.if_ (translateExpression c) (translateExpression a) (translateExpression b)
    translateExpression (B.Equals s a b)  = F.equals s (translateExpression a) (translateExpression b)
    translateExpression (B.FunApp f args) = F.application f (map translateExpression args)
    translateExpression (B.Ref lval)      = maybe n (F.select n) is
      where (n, is) = translateLValue lval

    translateLValue :: B.LValue -> (F.Term, Maybe (NonEmpty F.Term))
    translateLValue (B.LValue n is) = (n', is')
      where
        n' = F.constant (translateConstant n)
        is' = fmap (fmap translateExpression . sconcat) (NE.nonEmpty is)

    translateConstant :: B.Var -> F.Identifier
    translateConstant = fmap F.name
