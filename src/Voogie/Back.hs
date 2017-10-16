module Voogie.Back where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)

import Voogie.Theory
import qualified Voogie.FOOL.Smart as F

import qualified Voogie.Boogie as B
import Voogie.Boogie.Pretty()

import qualified Voogie.TPTP as TPTP
import Voogie.TPTP

data TranslationOptions =
  TranslationOptions { useArrayTheory :: Bool
                     }

translate :: TranslationOptions -> B.Boogie -> TPTP
translate opts (B.Boogie decls (B.Main pre stmts post)) = TPTP (signature ++ axioms ++ [conjecture])
  where
    signature  = fmap TPTP.Type  decls
    axioms     = fmap TPTP.Axiom pre
    conjecture = TPTP.Conjecture $ case NE.nonEmpty post of
      Nothing   -> F.booleanConstant True
      Just post -> foldr (either (translateStatement opts) (translateAssume opts))
                         (foldr1 (F.binary And) post) stmts


updates :: B.Statement -> NonEmpty B.Var
updates = NE.nub . updates'
  where
    updates' (B.If _ _ a b) = sconcat $ fmap updates' (foldl (flip NE.cons) a b)
    updates' (B.Assign ass) = fmap (B.lvariable . fst) ass

translateStatement :: TranslationOptions -> B.Statement -> F.Term -> F.Term
translateStatement opts s = F.let_ (F.Binding (F.tupleD vars) (body s))
  where
    vars = updates s

    body :: B.Statement -> F.Term
    body (B.Assign ass) = F.tupleLiteral (fmap analyzeAssignment vars)
      where
        assignments = NE.zip (fmap (B.lvariable . fst) ass) ass
        analyzeAssignment :: B.Var -> F.Term
        analyzeAssignment v = maybe (F.constant v) (uncurry translateAssign) maybeAssign
          where maybeAssign = lookup v (NE.toList assignments)
    body (B.If c f a b) = if f then F.if_ c' b' a' else F.if_ c' a' b'
      where
        tuple = F.tupleLiteral (fmap F.constant vars)
        a' = foldr (translateStatement opts) tuple a
        b' = foldr (translateStatement opts) tuple b
        c' = translateExpression c

translateAssign :: B.LValue -> B.Expression -> F.Term
translateAssign (B.LValue n is) e = translateAssign' var is'
  where
    var = F.constant (translateConstant n)
    is' = fmap (fmap translateExpression) is

    translateAssign' :: F.Term -> [NonEmpty F.Term] -> F.Term
    translateAssign' _ []     = translateExpression e
    translateAssign' a (i:is) = F.store a i (translateAssign' (F.select a i) is)

translateAssume :: TranslationOptions -> B.Assume -> F.Term -> F.Term
translateAssume _opts (B.Assume f) = F.binary Imply f

translateExpression :: B.Expression -> F.Term
translateExpression (B.IntegerLiteral i) = F.integerConstant i
translateExpression (B.BooleanLiteral b) = F.booleanConstant b
translateExpression (B.Unary  op e)   = F.unary  op (translateExpression e)
translateExpression (B.Binary op a b) = F.binary op (translateExpression a) (translateExpression b)
translateExpression (B.IfElse c a b)  = F.if_ (translateExpression c) (translateExpression a) (translateExpression b)
translateExpression (B.Equals s a b)  = F.equals s (translateExpression a) (translateExpression b)
translateExpression (B.FunApp f args) = F.application f (map translateExpression args)
translateExpression (B.Ref lval)      = translateLValue lval

translateLValue :: B.LValue -> F.Term
translateLValue (B.LValue n is) = foldl F.select var is'
  where
    var = F.constant (translateConstant n)
    is' = fmap (fmap translateExpression) is

translateConstant :: B.Var -> F.Identifier
translateConstant = fmap F.name

translateVar :: B.Var -> Typed F.Var
translateVar = fmap F.var
