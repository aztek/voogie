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


getBehaviourFunDef :: P.FunDef -> Behaviour
getBehaviourFunDef (P.FunDef _ _ vars ts) = getBehaviourTerminating ts'
  where
    ts' = case ts of
      P.Return ss e    -> P.Return (f ss) e
      P.IteReturn ss c a b -> P.IteReturn (f ss) c a b

    f :: P.NonTerminating -> P.NonTerminating
    f (P.NonTerminating ss) = P.NonTerminating (map P.Declare vars ++ ss)



foldFunDefs :: F.Term -> [P.FunDef] -> F.Term
foldFunDefs = foldr bind
  where
    bind :: P.FunDef -> F.Term -> F.Term
    bind fd@(P.FunDef t f args ts) = F.let_ (F.Binding def renaming)
      where
        def = F.Symbol (Typed f t) (fmap translateVar args)
        beh = getBehaviourFunDef fd
        body = translateTerminating ts beh --(Behaviour Nothing (declares beh) (updates beh))
        renaming = case NE.nonEmpty args of
          Nothing   -> body
          Just args -> F.let_ (F.Binding def tuple) body
            where
              def   = F.tupleD (fmap translateConstant args)
              tuple = F.tupleLiteral (fmap (F.variable . translateVar) args)


right :: Type -> [F.Term] -> F.Term
right typ ts = case NE.nonEmpty ts of
  Nothing  -> F.nothing typ
  Just ts' -> F.right typ (F.tupleLiteral ts')

left :: F.Term -> [Type] -> F.Term
left t types = case NE.nonEmpty types of
  Nothing     -> F.just t
  Just types' -> F.left t (tupleType types')


translateTerminating :: P.Terminating -> Behaviour -> F.Term
translateTerminating t beh = foldNonTerminating returnValue nt beh
  where
    (nt, returnValue) = case t of
      P.Return nt e -> (nt, returnValue)
        where
          e' = translateExpression e
          returnValue = case returns beh of
            Nothing -> e'
            Just _  -> left e' (fmap typeOf $ S.toList $ updates beh)
      P.IteReturn nt c a b -> (nt, F.if_ c' a' b')
        where
          c' = translateExpression  c
          a' = translateTerminating a beh
          b' = translateTerminating b beh

foldNonTerminating :: F.Term -> P.NonTerminating -> Behaviour -> F.Term
foldNonTerminating t nt@(P.NonTerminating ss) behaviour = foldr process t ss
  where
    u = S.toList $ updated (getBehaviourNonTerminating nt)

    process :: P.Statement -> F.Term -> F.Term
    process (P.Declare _) = id
    process (P.Assign lval e) = F.let_ (F.Binding (F.Symbol c []) body)
      where
        e' = translateExpression e
        c = translateConstant $ case lval of
          P.Variable  v   -> v
          P.ArrayElem v _ -> v
        body = case lval of
          P.Variable  _   -> e'
          P.ArrayElem _ a -> F.store (F.constant c) (translateExpression a) e'
    process (P.If c a (Left b)) = case NE.nonEmpty u of
      Nothing -> id
      Just u  -> F.let_ (F.Binding def body)
        where
          body = F.if_ c' a' b'

          c' = translateExpression c
          a' = foldNonTerminating ctx a behaviour
          b' = foldNonTerminating ctx b behaviour

          def = F.tupleD u
          
          tupleLiteral = F.tupleLiteral $ fmap F.constant u
          ctx = case returns behaviour of
            Nothing  -> tupleLiteral
            Just typ -> right typ (NE.toList $ fmap F.constant u)

    process (P.If c a (Right (flp, b))) = F.let_ (F.Binding def body) . unbind
      where
        body = if flp then F.if_ c' b' a' else F.if_ c' a' b'

        c' = translateExpression c
        a' = foldNonTerminating ctx a behaviour
        b' = translateTerminating b behaviour

        typ = fromJust (returns behaviour)

        tupleLiteral = F.tupleLiteral $ NE.fromList $ fmap F.constant u
        ctx = case returns behaviour of
          Nothing  -> tupleLiteral
          Just typ -> right typ (fmap F.constant u)

        def = F.Symbol symbol []
        symbol = Typed "i" (typeOf body)
        constant = F.constant symbol

        unbind = case NE.nonEmpty u of
          Nothing -> F.if_ (F.isJust constant) returnValue
            where
              returnValue = F.fromJust constant
          Just vars -> F.if_ (F.isLeft constant)
                             returnValue .
                             F.let_ (F.Binding (F.tupleD vars) (F.fromRight constant))
            where
              returnValue = F.fromLeft constant


translate :: P.Program -> (Signature, F.Formula)
translate (P.Program fs ss as) = case NE.nonEmpty as of
  Nothing -> (S.empty, F.booleanConstant True)
  Just as -> (signature, conjecture)
    where
      b = getBehaviourNonTerminating ss

      signature = declares b \\ nonArrays (updated b)
      nonArrays = S.filter (not . isArray . typeOf)

      conjecture = foldFunDefs (foldNonTerminating assert ss initBehaviour) fs
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