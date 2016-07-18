module Kyckling.Back where

import Data.Char
import Data.Either
import qualified Data.Set as S
import Data.Set (Set, (\\), union)

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Kyckling.Theory
import qualified Kyckling.FOOL.Smart as F
import qualified Kyckling.FOOL.Tuple as F.Tuple

import qualified Kyckling.Program as P

data Definition = Symbol P.Function [P.Var]
                | TupleD (F.Tuple.Tuple P.Var)

tupleD :: NonEmpty P.Var -> Definition
tupleD = either (flip Symbol []) TupleD . F.Tuple.nonUnit

data Binding = Regular Definition F.Term
             | MaybeBinding F.Term
             | EitherBinding Definition F.Term

boundBy :: [Binding] -> Set F.Identifier
boundBy = S.unions . fmap boundByBinding
  where
    boundByBinding :: Binding -> Set F.Identifier
    boundByBinding (Regular def _) = boundByDefinition def
    boundByBinding (MaybeBinding _) = S.empty
    boundByBinding (EitherBinding def _) = boundByDefinition def

    boundByDefinition :: Definition -> Set F.Identifier
    boundByDefinition (Symbol c _) = S.singleton c
    boundByDefinition (TupleD cs)  = S.fromList (F.Tuple.toList cs)

translate :: P.Program -> (Signature, F.Formula)
translate (P.Program fs ss as) = case NE.nonEmpty as of
  Nothing -> (S.empty, F.booleanConstant True)
  Just as -> (signature, conjecture)
    where
      funBindings = map translateFunDef fs
      (declared, bindings) = translateStatements ss

      nonArrays = S.filter (not . isArray . typeOf)

      signature = declared \\ nonArrays (boundBy bindings)

      assert = foldr1 (F.binary And) (fmap (\(P.Assertion f) -> f) as)
      conjecture = foldBindings assert (funBindings ++ bindings)

foldBindings :: F.Term -> [Binding] -> F.Term
foldBindings = foldr bind
  where
    bind :: Binding -> F.Term -> F.Term
    bind (Regular def  body) = let_ def body
    bind (MaybeBinding body) = let_ freshDef body .
                                    F.if_ (F.isJust constant) (F.fromJust constant)
      where
        (freshDef, constant) = name body
    bind (EitherBinding def body) = let_ freshDef body .
                                         F.if_ (F.isLeft constant)
                                               (F.fromLeft constant) .
                                               let_ def (F.fromRight constant)
      where
        (freshDef, constant) = name body

    name :: F.Term -> (Definition, F.Term)
    name body = (Symbol symbol [], F.constant symbol) where symbol = Typed "i" (typeOf body)

    let_ :: Definition -> F.Term -> F.Term -> F.Term
    let_ (Symbol f args) body term = F.let_ (F.Binding def renaming) term
      where
        def = F.Symbol f (fmap translateVar args)
        renaming = case NE.nonEmpty args of
                     Nothing   -> body
                     Just args -> F.let_ (F.Binding def tuple) body
                       where
                        def   = F.tupleD (fmap translateConstant args)
                        tuple = F.tupleLiteral (fmap (F.variable . translateVar) args)
    let_ (TupleD vars) body term = F.let_ (F.Binding (F.TupleD vars) body) term

type Declaration = F.Identifier

translateStatements :: [P.Statement] -> (Set Declaration, [Binding])
translateStatements ss = (S.fromList ds, bs)
  where
    (ds, bs) = partitionEithers (map translateStatement ss)

translateStatement :: P.Statement -> Either Declaration Binding
translateStatement (P.Declare v) = Left (translateConstant v)
translateStatement (P.Assign lval e) = Right binding
  where
    e' = translateExpression e
    n  = translateConstant (var lval)

    var (P.Variable  v)   = v
    var (P.ArrayElem v _) = v

    body = case lval of
             P.Variable  _   -> e'
             P.ArrayElem _ a -> F.store (F.constant n) (translateExpression a) e'

    binding = Regular (Symbol n []) body
translateStatement (P.If c a b) = Right binding
  where
    c' = translateExpression c

    (thenDeclared, thenBindings) = translateStatements a
    (elseDeclared, elseBindings) = either translateStatements (const (S.empty, [])) b

    -- TODO: for now we assume that there are no unbound declarations;
    --       this assumption must be removed in the future

    undeclared = S.toList (bound \\ declared)
      where
        bound = boundBy thenBindings `union` boundBy elseBindings
        declared = thenDeclared `union` elseDeclared

    binding = case b of
      Left b -> Regular (tupleD updated) (F.if_ c' thenBranch elseBranch)
        where
          updated = NE.fromList undeclared
          updatedTerm = F.tupleLiteral (fmap F.constant updated)
          thenBranch = foldBindings updatedTerm thenBindings
          elseBranch = foldBindings updatedTerm elseBindings

      Right (flp, b) ->
        case NE.nonEmpty undeclared of
          Nothing -> MaybeBinding (ite thenBranch elseBranch)
            where
              thenBranch = foldBindings (F.nothing returnType) thenBindings
              elseBranch = F.just returnTerm

          Just updated -> EitherBinding (tupleD updated) (ite thenBranch elseBranch)
            where
              updatedTerm = F.tupleLiteral (fmap F.constant updated)
              thenBranch = foldBindings (F.right returnType updatedTerm) thenBindings
              elseBranch = F.left returnTerm (typeOf updatedTerm)
        where
          ite = if flp then F.if_ c' else flip (F.if_ c')
          returnTerm = translateTerminating b
          returnType = typeOf returnTerm

translateFunDef :: P.FunDef -> Binding
translateFunDef (P.FunDef t f vars ts) = Regular symbol (translateTerminating ts)
  where
    symbol = Symbol (Typed f t) vars

translateTerminating :: P.TerminatingStatement -> F.Term
translateTerminating (P.Return    ss e)     = foldBindings (translateExpression e) (snd $ translateStatements ss)
translateTerminating (P.IteReturn ss c a b) = foldBindings (F.if_ c' a' b')        (snd $ translateStatements ss)
  where
    a' = translateTerminating a
    b' = translateTerminating b
    c' = translateExpression c

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