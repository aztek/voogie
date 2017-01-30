module Voogie.Back where

import Data.Either
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set, (\\), union)
import Data.Bifunctor

import qualified Data.List.NonEmpty as NE

import Control.Applicative

import Voogie.Theory
import qualified Voogie.FOOL.Smart as F
import qualified Voogie.FOOL.Tuple as F.Tuple

import qualified Voogie.Boogie as P
import Voogie.Boogie.Behaviour
import Voogie.Boogie.Pretty

return_ :: Behaviour -> F.Term -> F.Term
return_ b t = case returns b of
  Nothing -> t
  Just _  -> case NE.nonEmpty (S.toList $ updates b) of
    Nothing -> F.some t
    Just u  -> F.left t (tupleType $ fmap typeOf u)

context :: Behaviour -> F.Term
context b = case NE.nonEmpty (S.toList $ updates b) of
  Nothing -> case returns b of
    Nothing -> error "invariant violation"
    Just t  -> F.none t
  Just ts -> maybe lit (flip F.right lit) (returns b)
    where lit = F.tupleLiteral (fmap F.constant ts)

foldFunDefs :: F.Term -> [P.FunDef] -> F.Term
foldFunDefs = foldr bind
  where
    bind :: P.FunDef -> F.Term -> F.Term
    bind (P.FunDef f args ts) = F.let_ (F.Binding def renaming)
      where
        def = F.Symbol f (fmap translateVar args)
        body = translateTerminating initBehaviour ts
        renaming = case NE.nonEmpty args of
          Nothing   -> body
          Just args -> F.let_ (F.binding $ fmap trans args) body
            where
              trans = (,) <$> translateConstant <*> F.variable . translateVar

translateTerminating :: Behaviour -> P.Terminating -> F.Term
translateTerminating returnBehaviour ts@(P.Terminating ss (P.Scoped _ r)) =
  foldr (translateStatement (getBehaviourTerminating ts))
        (translateReturn returnBehaviour r) ss

translateReturn :: Behaviour -> P.Return -> F.Term
translateReturn topLevelBehaviour (P.Return e) = return_ topLevelBehaviour (translateExpression e)
translateReturn topLevelBehaviour (P.IteReturn c a b) = F.if_ c' a' b'
  where
   c' = translateExpression  c
   a' = translateTerminating topLevelBehaviour a
   b' = translateTerminating topLevelBehaviour b

translateStatement :: Behaviour -> P.Scoped P.Statement -> F.Term -> F.Term
translateStatement _ (P.Scoped _ (P.Assign ass)) = F.let_ (F.binding (fmap translateAssignment ass))
translateStatement topLevelBehaviour ite@(P.Scoped _ (P.If c a b)) =
  F.let_ (F.Binding def body) . unbind
  where
    c' = translateExpression c
    a' = foldr (translateStatement beh) (context beh) a
    b' = either (foldr (translateStatement beh) (context beh))
                (translateTerminating beh . snd) b

    body = case b of
      Right (True, _) -> F.if_ c' b' a'
      _               -> F.if_ c' a' b'

    behaviour = getBehaviour ite
    (beh, def, unbind) = if isLeft b && isNothing (returns behaviour)
                         then let def = case NE.nonEmpty (S.toList $ updates behaviour) of
                                          Nothing -> error "invariant violation"
                                          Just vars -> F.tupleD vars
                               in (behaviour, def, id)
                         else let def = F.Symbol symbol []
                                  symbol = Typed "i" (typeOf a')
                                  constant = F.constant symbol
                                  unbind = case NE.nonEmpty (S.toList $ updates topLevelBehaviour) of
                                    Nothing   -> F.if_ (F.isSome constant)
                                                       (return_ topLevelBehaviour (F.fromSome constant))
                                    Just vars -> F.if_ (F.isLeft constant)
                                                       (return_ topLevelBehaviour (F.fromLeft constant)) .
                                                       F.let_ (F.Binding (F.tupleD vars) (F.fromRight constant))
                               in (topLevelBehaviour, def, unbind)

translateAssignment :: (P.LValue, P.Expression) -> (F.Identifier, F.Term)
translateAssignment (lval, e) = (c, body)
  where
    c = translateConstant (P.lvariable lval)
    e' = translateExpression e
    body = case lval of
      P.Variable  _   -> e'
      P.ArrayElem _ a -> F.store (F.constant c) (translateExpression a) e'

translate :: P.Boogie -> (Signature, F.Formula)
translate (P.Boogie fs ss as) = case NE.nonEmpty as of
  Nothing -> (S.empty, F.booleanConstant True)
  Just as -> (signature, conjecture)
    where
      b = getBehaviourNonTerminating ss

      signature = collectDeclarations ss \\ nonArrays (updates b)
      nonArrays = S.filter (not . isArray . typeOf)

      conjecture = foldFunDefs (foldr (\s -> translateStatement (getBehaviour s) s) assert ss) fs
      assert = foldr1 (F.binary And) (fmap (\(P.Assertion f) -> f) as)


translateExpression :: P.Expression -> F.Term
translateExpression (P.IntegerLiteral i) = F.integerConstant i 
translateExpression (P.BooleanLiteral b) = F.booleanConstant b
translateExpression (P.Unary  op e)   = F.unary  op (translateExpression e)
translateExpression (P.Binary op a b) = F.binary op (translateExpression a) (translateExpression b)
translateExpression (P.IfElse c a b)  = F.if_ (translateExpression c) (translateExpression a) (translateExpression b)
translateExpression (P.Equals s a b)  = F.equals s (translateExpression a) (translateExpression b)
translateExpression (P.FunApp f args) = F.application f (map translateExpression args)
translateExpression (P.Ref lval)      = translateLValue lval

translateLValue :: P.LValue -> F.Term
translateLValue (P.Variable v)    = F.constant (translateConstant v)
translateLValue (P.ArrayElem v e) = F.select (F.constant (translateConstant v)) (translateExpression e)

translateConstant :: P.Var -> F.Identifier
translateConstant = fmap F.name

translateVar :: P.Var -> Typed F.Var
translateVar = fmap F.var
