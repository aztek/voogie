module Kyckling.Back where

import Data.Char
import Data.Either
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set, (\\), union)

import qualified Data.List.NonEmpty as NE

import Kyckling.Theory
import qualified Kyckling.FOOL.Smart as F
import qualified Kyckling.FOOL.Tuple as F.Tuple

import qualified Kyckling.Program as P
import Kyckling.Program.Behaviour

return_ :: Behaviour -> F.Term -> F.Term
return_ b t = case returns b of
  Nothing -> t
  Just _  -> case NE.nonEmpty (S.toList $ updates b) of
    Nothing -> F.some t
    Just u  -> F.left t (tupleType $ fmap typeOf u)

context :: Behaviour -> F.Term
context b = case (returns b, NE.nonEmpty $ S.toList $ updates b) of
  (_,       Nothing) -> error "invariant violation"
  (Nothing, Just ts) -> F.tupleLiteral (fmap F.constant ts)
  (Just t,  Just ts) -> F.right t (F.tupleLiteral $ fmap F.constant ts)

foldFunDefs :: F.Term -> [P.FunDef] -> F.Term
foldFunDefs = foldr bind
  where
    bind :: P.FunDef -> F.Term -> F.Term
    bind fd@(P.FunDef f args ts) = F.let_ (F.Binding def renaming)
      where
        def = F.Symbol f (fmap translateVar args)
        body = translateTerminating initBehaviour ts
        renaming = case NE.nonEmpty args of
          Nothing   -> body
          Just args -> F.let_ (F.Binding def tuple) body
            where
              def   = F.tupleD (fmap translateConstant args)
              tuple = F.tupleLiteral (fmap (F.variable . translateVar) args)

translateTerminating :: Behaviour -> P.Terminating -> F.Term
translateTerminating returnBehaviour ts@(P.Terminating ss r) =
  foldr (translateStatement (getBehaviourTerminating ts)) (translateReturn returnBehaviour r) ss

translateReturn :: Behaviour -> P.Return -> F.Term
translateReturn topLevelBehaviour (P.Return e) = return_ topLevelBehaviour (translateExpression e)
translateReturn topLevelBehaviour (P.IteReturn c a b) = F.if_ c' a' b'
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
translateStatement topLevelBehaviour ite@(P.If c a (Left b)) = F.let_ (F.Binding def body) . unbind
  where
    c' = translateExpression c
    a' = foldr (translateStatement beh) (context beh) a
    b' = foldr (translateStatement beh) (context beh) b

    body = F.if_ c' a' b'

    behaviour = getBehaviour ite
    (beh, def, unbind) = case returns behaviour of
      Nothing -> (behaviour, def, id)
        where
          def = case NE.nonEmpty (S.toList $ updated behaviour) of
            Nothing -> error "invariant violation"
            Just vars -> F.tupleD vars
      Just _ -> (topLevelBehaviour, def, unbind)
        where
          def = F.Symbol symbol []
          symbol = Typed "i" (typeOf a')
          constant = F.constant symbol

          unbind = case NE.nonEmpty (S.toList $ updated topLevelBehaviour) of
            Nothing   -> F.if_ (F.isSome constant)
                               (return_ topLevelBehaviour (F.fromSome constant))
            Just vars -> F.if_ (F.isLeft constant)
                               (return_ topLevelBehaviour (F.fromLeft constant)) .
                               F.let_ (F.Binding (F.tupleD vars) (F.fromRight constant))
translateStatement topLevelBehaviour (P.If c a (Right (flp, b))) = F.let_ (F.Binding def body) . unbind
  where
    c' = translateExpression c
    a' = foldr (translateStatement topLevelBehaviour) (context topLevelBehaviour) a
    b' = translateTerminating topLevelBehaviour b

    def = F.Symbol symbol []
    symbol = Typed "i" (typeOf a')
    constant = F.constant symbol

    body = if flp then F.if_ c' b' a' else F.if_ c' a' b'

    unbind = case NE.nonEmpty (S.toList $ updated topLevelBehaviour) of
      Nothing   -> F.if_ (F.isSome constant)
                         (return_ topLevelBehaviour (F.fromSome constant))
      Just vars -> F.if_ (F.isLeft constant)
                         (return_ topLevelBehaviour (F.fromLeft constant)) .
                         F.let_ (F.Binding (F.tupleD vars) (F.fromRight constant))

translate :: P.Program -> (Signature, F.Formula)
translate (P.Program fs ss as) = case NE.nonEmpty as of
  Nothing -> (S.empty, F.booleanConstant True)
  Just as -> (signature, conjecture)
    where
      b = getBehaviourNonTerminating ss

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