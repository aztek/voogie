module Kyckling.Back where

import Data.Char
import Data.Either
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set, (\\), union)

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Control.Applicative

import Kyckling.Theory
import qualified Kyckling.FOOL.Smart as F
import qualified Kyckling.FOOL.Tuple as F.Tuple

import qualified Kyckling.Program as P

data Behaviour = Behaviour { returns :: Maybe Type, declares :: Set P.Var, updates :: Set P.Var }

initBehaviour :: Behaviour
initBehaviour = Behaviour Nothing S.empty S.empty

updated :: Behaviour -> Set P.Var
updated (Behaviour _ d u) = u \\ d

getBehaviour :: P.Statement -> Behaviour
getBehaviour (P.Declare var)   = Behaviour Nothing (S.singleton var) S.empty
getBehaviour (P.Assign lval e) = Behaviour Nothing S.empty (S.singleton var)
  where
    var = case lval of
      P.Variable  v   -> v
      P.ArrayElem v _ -> v
getBehaviour (P.If c a b) = Behaviour r d u
  where
    a' = getBehaviourNonTerminating a
    b' = either getBehaviourNonTerminating (getBehaviourTerminating . snd) b

    d = S.empty
    r = returns a' <|> returns b'
    u = updated a' `union` updated b'

getBehaviourTerminating :: P.Terminating -> Behaviour
getBehaviourTerminating (P.Return ss e) = Behaviour (Just $ typeOf e) d u
  where
    Behaviour _ d u = getBehaviourNonTerminating ss
getBehaviourTerminating (P.IteReturn ss _ a b) = Behaviour r d u
  where
    a' = getBehaviourTerminating a
    b' = getBehaviourTerminating b

    d = S.empty
    r = returns a' <|> returns b'
    u = updated a' `union` updated b'

getBehaviourNonTerminating :: P.NonTerminating -> Behaviour
getBehaviourNonTerminating (P.NonTerminating ss) = foldr (merge . getBehaviour) (Behaviour Nothing S.empty S.empty) ss
  where
    merge :: Behaviour -> Behaviour -> Behaviour
    merge (Behaviour r1 d1 u1) (Behaviour r2 d2 u2) = Behaviour (r1 <|> r2) (d1 `union` d2) (u1 `union` u2)


foldFunDefs :: F.Term -> [P.FunDef] -> F.Term
foldFunDefs = foldr bind
  where
    bind :: P.FunDef -> F.Term -> F.Term
    bind fd@(P.FunDef t f args ts) = F.let_ (F.Binding def renaming)
      where
        def = F.Symbol (Typed f t) (fmap translateVar args)
        body = translateTerminating (getBehaviourTerminating ts) ts
        renaming = case NE.nonEmpty args of
          Nothing   -> body
          Just args -> F.let_ (F.Binding def tuple) body
            where
              def   = F.tupleD (fmap translateConstant args)
              tuple = F.tupleLiteral (fmap (F.variable . translateVar) args)


return_ :: Behaviour -> F.Term -> F.Term
return_ b t = case returns b of
  Nothing -> t
  Just _  -> case NE.nonEmpty (S.toList $ updates b) of
    Nothing -> F.just t
    Just u  -> F.left t (tupleType $ fmap typeOf u)

context :: Behaviour -> F.Term
context b = case (returns b, NE.nonEmpty $ S.toList $ updates b) of
  (_,       Nothing) -> error "invariant violation"
  (Nothing, Just ts) -> F.tupleLiteral (fmap F.constant ts)
  (Just t,  Just ts) -> F.right t (F.tupleLiteral $ fmap F.constant ts)


translateTerminating :: Behaviour -> P.Terminating -> F.Term
translateTerminating topLevelBehaviour t = foldr (translateStatement topLevelBehaviour) returnValue ss
  where
    (ss, returnValue) = case t of
      P.Return    (P.NonTerminating ss) e     -> (ss, return_ topLevelBehaviour $ translateExpression e)
      P.IteReturn (P.NonTerminating ss) c a b -> (ss, F.if_ c' a' b')
        where
          c' = translateExpression  c
          a' = translateTerminating topLevelBehaviour a
          b' = translateTerminating topLevelBehaviour b

translateStatement :: Behaviour -> P.Statement -> F.Term -> F.Term
translateStatement _ (P.Declare _) = id
translateStatement _ (P.Assign lval e) = F.let_ (F.Binding (F.Symbol c []) body)
  where
    e' = translateExpression e
    c = translateConstant $ case lval of
      P.Variable  v   -> v
      P.ArrayElem v _ -> v
    body = case lval of
      P.Variable  _   -> e'
      P.ArrayElem _ a -> F.store (F.constant c) (translateExpression a) e'
translateStatement topLevelBehaviour (P.If c (P.NonTerminating a) b) = F.let_ (F.Binding def body) . unbind
  where
    c' = translateExpression c
    a' = foldr (translateStatement topLevelBehaviour) (context topLevelBehaviour) a
    b' = either (\(P.NonTerminating ss) -> foldr (translateStatement topLevelBehaviour) (context topLevelBehaviour) ss)
                (translateTerminating topLevelBehaviour . snd) b

    def = F.Symbol symbol []
    symbol = Typed "i" (typeOf a')
    constant = F.constant symbol

    body = case b of
      Right (True, b) -> F.if_ c' b' a'
      _               -> F.if_ c' a' b'

    unbind = case NE.nonEmpty (S.toList $ updated topLevelBehaviour) of
      Nothing   -> F.if_ (F.isJust constant)
                         (return_ topLevelBehaviour (F.fromJust constant))
      Just vars -> F.if_ (F.isLeft constant)
                         (return_ topLevelBehaviour (F.fromLeft constant)) .
                         F.let_ (F.Binding (F.tupleD vars) (F.fromRight constant))

translate :: P.Program -> (Signature, F.Formula)
translate (P.Program fs (P.NonTerminating ss) as) = case NE.nonEmpty as of
  Nothing -> (S.empty, F.booleanConstant True)
  Just as -> (signature, conjecture)
    where
      b = getBehaviourNonTerminating (P.NonTerminating ss)

      signature = declares b \\ nonArrays (updated b)
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
translateConstant = fmap (map toLower)

translateVar :: P.Var -> Typed F.Var
translateVar = fmap (F.Var . map toUpper)