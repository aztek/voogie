module Kyckling.Back where

import Data.Char
import Data.Either
import qualified Data.Set as S
import Data.Set (Set, (\\), union)

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Kyckling.Theory
import qualified Kyckling.FOOL as F
import qualified Kyckling.FOOL.Tuple as F.Tuple

import qualified Kyckling.Program as P

data Binding = Regular F.Definition F.Term
             | MaybeBinding F.Term
             | EitherBinding F.Definition F.Term

boundBy :: [Binding] -> Set F.Identifier
boundBy = S.unions . fmap boundByBinding
  where
    boundByBinding :: Binding -> Set F.Identifier
    boundByBinding (Regular def _) = boundByDefinition def
    boundByBinding (MaybeBinding _) = S.empty
    boundByBinding (EitherBinding def _) = boundByDefinition def

    boundByDefinition :: F.Definition -> Set F.Identifier
    boundByDefinition (F.Symbol c _) = S.singleton c
    boundByDefinition (F.TupleD cs)  = S.fromList (F.Tuple.toList cs)

translate :: P.Program -> (Signature, F.Formula)
translate (P.Program fs ss as) = case NE.nonEmpty as of
  Nothing -> (S.empty, F.BooleanConstant True)
  Just as -> (signature, conjecture)
    where
      funBindings = map translateFunDef fs
      (declared, bindings) = translateStatements ss

      nonArrays = S.filter (not . isArray . typeOf)

      signature = declared \\ nonArrays (boundBy bindings)

      assert = foldr1 (F.Binary And) (fmap (\(P.Assertion f) -> f) as)
      conjecture = foldBindings assert (funBindings ++ bindings)

foldBindings :: F.Term -> [Binding] -> F.Term
foldBindings = foldr f
  where
    f (Regular def b) = F.Let (F.Binding def b)
    f (MaybeBinding b) = F.Let maybeBinding . F.If (F.IsJust constant) (F.FromJust constant)
      where
        maybeBinding = F.Binding (F.Symbol symbol []) b
        symbol = Typed "i" (typeOf b)
        constant = F.Application symbol []
    f (EitherBinding def b) = F.Let eitherBinding . F.If (F.IsLeft constant) (F.FromLeft constant) . F.Let defBinding
      where
        eitherBinding = F.Binding (F.Symbol symbol []) b
        symbol = Typed "i" (typeOf b)
        constant = F.Application symbol []

        defBinding = F.Binding def (F.FromRight constant)

type Declaration = F.Identifier

translateStatements :: [P.Statement] -> (Set Declaration, [Binding])
translateStatements ss = (S.fromList ds, bs)
  where
    (ds, bs) = partitionEithers (map translateStatement ss)

translateStatement :: P.Statement -> Either Declaration Binding
translateStatement (P.Declare v) = Left (translateVar v)
translateStatement (P.Assign lval e) = Right binding
  where
    e' = translateExpression e
    n  = translateVar (var lval)

    var (P.Variable  v)   = v
    var (P.ArrayElem v _) = v

    body = case lval of
             P.Variable  _   -> e'
             P.ArrayElem _ a -> F.Store (F.Application n []) (translateExpression a) e'

    binding = Regular (F.Symbol n []) body
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
      Left b -> Regular (toDefinition updated) (F.If c' thenBranch elseBranch)
        where
          updated = NE.fromList undeclared
          updatedTerm = toTerm updated
          thenBranch = foldBindings updatedTerm thenBindings
          elseBranch = foldBindings updatedTerm elseBindings

      Right (flp, b) ->
        case NE.nonEmpty undeclared of
          Nothing -> MaybeBinding (ite thenBranch elseBranch)
            where
              thenBranch = foldBindings (F.Nothing_ returnType) thenBindings
              elseBranch = F.Just_ returnTerm

          Just updated -> EitherBinding (toDefinition updated) (ite thenBranch elseBranch)
            where
              updatedTerm = toTerm updated
              thenBranch = foldBindings (F.Right_ returnType updatedTerm) thenBindings
              elseBranch = F.Left_ returnTerm (typeOf updatedTerm)
        where
          ite = if flp then F.If c' else flip (F.If c')
          returnTerm = translateTerminating b
          returnType = typeOf returnTerm

    toDefinition :: NonEmpty F.Identifier -> F.Definition
    toDefinition = either (flip F.Symbol []) F.TupleD . F.Tuple.nonUnit

    toTerm :: NonEmpty F.Identifier -> F.Term
    toTerm = either (flip F.Application []) (F.TupleLiteral . fmap (flip F.Application [])) . F.Tuple.nonUnit

translateFunDef :: P.FunDef -> Binding
translateFunDef (P.FunDef t f vars ts) = Regular symbol (translateTerminating ts)
  where
    symbol = F.Symbol (Typed f t) (map (fmap F.Var) vars)

translateTerminating :: P.TerminatingStatement -> F.Term
translateTerminating (P.Return    ss e)     = foldBindings (translateExpression e) (snd $ translateStatements ss)
translateTerminating (P.IteReturn ss c a b) = foldBindings (F.If c' a' b')         (snd $ translateStatements ss)
  where
    a' = translateTerminating a
    b' = translateTerminating b
    c' = translateExpression c

translateExpression :: P.Expression -> F.Term
translateExpression (P.IntegerLiteral i) = F.IntegerConstant i 
translateExpression (P.BooleanLiteral b) = F.BooleanConstant b
translateExpression (P.Unary  op e)   = F.Unary  op (translateExpression e)
translateExpression (P.Binary op a b) = F.Binary op (translateExpression a) (translateExpression b)
translateExpression (P.IfElse c a b)  = F.If (translateExpression c) (translateExpression a) (translateExpression b)
translateExpression (P.Equals s a b)  = F.Equals s (translateExpression a) (translateExpression b)
translateExpression (P.FunApp f args) = F.Application f (map translateExpression args)
translateExpression (P.Ref lval)      = translateLValue lval

translateLValue :: P.LValue -> F.Term
translateLValue (P.Variable v)    = F.Application (translateVar v) []
translateLValue (P.ArrayElem v e) = F.Select (F.Application (translateVar v) []) (translateExpression e)

translateVar :: P.Var -> F.Identifier
translateVar (Typed v t) = Typed (map toLower v) t