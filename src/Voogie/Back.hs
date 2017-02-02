module Voogie.Back where

import Data.Either
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set, (\\), union, unions)
import Data.Bifunctor

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Control.Applicative

import Data.Semigroup (sconcat)

import Voogie.Theory
import qualified Voogie.FOOL.Smart as F
import qualified Voogie.FOOL.Tuple as F.Tuple

import qualified Voogie.Boogie as B
import Voogie.Boogie.Pretty

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
      Just post -> foldr (translateStatement opts) (foldr1 (F.binary And) post) stmts


updates :: B.Statement -> NonEmpty B.Var
updates (B.If _ _ a b) = NE.nub $ sconcat $ fmap updates (foldl (flip NE.cons) a b)
updates (B.Assign ass) = NE.nub $ fmap (B.lvariable . fst) ass

translateStatement :: TranslationOptions -> B.Statement -> F.Term -> F.Term
translateStatement opts s = F.let_ (F.Binding (F.tupleD vars) (body s))
  where
    vars = updates s

    body :: B.Statement -> F.Term
    body (B.Assign ass) = F.tupleLiteral (translateAssignments opts ass)
    body (B.If c f a b) = if f then F.if_ c' b' a' else F.if_ c' a' b'
      where
        tuple = F.tupleLiteral (fmap F.constant vars)
        a' = foldr (translateStatement opts) tuple a
        b' = foldr (translateStatement opts) tuple b
        c' = translateExpression c


translateAssignments :: TranslationOptions -> NonEmpty (B.LValue, B.Expression) -> NonEmpty F.Term
translateAssignments opts = fmap translateAssignment
  where
    translateAssignment :: (B.LValue, B.Expression) -> F.Term
    translateAssignment (B.Variable    _, e) = translateExpression e
    translateAssignment (B.ArrayElem v a, e) = F.store (F.constant v) (translateExpression a) (translateExpression e)


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
translateLValue (B.Variable v)    = F.constant (translateConstant v)
translateLValue (B.ArrayElem v e) = F.select (F.constant (translateConstant v)) (translateExpression e)

translateConstant :: B.Var -> F.Identifier
translateConstant = fmap F.name

translateVar :: B.Var -> Typed F.Var
translateVar = fmap F.var
