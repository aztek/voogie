module Voogie.Back where

import Data.Either
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set, (\\), union, unions)
import Data.Bifunctor

import qualified Data.List.NonEmpty as NE

import Control.Applicative

import Voogie.Theory
import qualified Voogie.FOOL.Smart as F
import qualified Voogie.FOOL.Tuple as F.Tuple

import qualified Voogie.Boogie as B
import Voogie.Boogie.Pretty

import qualified Voogie.TPTP as TPTP
import Voogie.TPTP


translate :: B.Boogie -> TPTP
translate (B.Boogie decls (B.Main pre stmts post)) = TPTP (signature ++ axioms ++ [conjecture])
  where
    signature  = fmap TPTP.Type  decls
    axioms     = fmap TPTP.Axiom pre
    conjecture = TPTP.Conjecture $ case NE.nonEmpty post of
      Nothing   -> F.booleanConstant True
      Just post -> foldr translateStatement (foldr1 (F.binary And) post) stmts


updates :: B.Statement -> Set B.Var
updates (B.If _ _ a b) = unions (map updates (NE.toList a ++ b))
updates (B.Assign ass) = S.fromList $ map (B.lvariable . fst) (NE.toList ass)


translateStatement :: B.Statement -> F.Term -> F.Term
translateStatement (B.Assign ass) = F.let_ (F.binding (fmap translateAssignment ass))
translateStatement ite@(B.If c f a b) = F.let_ (F.Binding def body)
  where
    c' = translateExpression c
    (def, body) = case NE.nonEmpty (S.toList $ updates ite) of
      Nothing   -> undefined
      Just vars -> (F.tupleD vars, body)
        where
          body = if f then F.if_ c' b' a' else F.if_ c' a' b'
          a' = foldr translateStatement tuple (NE.toList a)
          b' = foldr translateStatement tuple b
          tuple = F.tupleLiteral (fmap F.constant vars)


translateAssignment :: (B.LValue, B.Expression) -> (F.Identifier, F.Term)
translateAssignment (lval, e) = (c, body)
  where
    c = translateConstant (B.lvariable lval)
    e' = translateExpression e
    body = case lval of
      B.Variable  _   -> e'
      B.ArrayElem _ a -> F.store (F.constant c) (translateExpression a) e'


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
