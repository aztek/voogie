module Kyckling.Back where

import Data.Char
import Data.Either
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set, (\\), union)

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Kyckling.Theory
import qualified Kyckling.FOOL.Smart as F
import qualified Kyckling.FOOL.Tuple as F.Tuple

import qualified Kyckling.Program as P

data Binding = Variables (NonEmpty P.Var) F.Term
             | Terminating [P.Var] F.Term

data FunctionBinding = FunctionBinding P.Function [P.Var] F.Term

boundBy :: [Binding] -> Set F.Identifier
boundBy = S.fromList . concatMap boundByBinding
  where
    boundByBinding :: Binding -> [F.Identifier]
    boundByBinding (Variables cs _) = NE.toList cs
    boundByBinding (Terminating vars _) = vars

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
      conjecture = foldFunctionBindings (foldBindings assert bindings) funBindings

foldBindings :: F.Term -> [Binding] -> F.Term
foldBindings = foldr bind
  where
    bind :: Binding -> F.Term -> F.Term
    bind (Variables vars  body) = F.let_ (F.Binding (F.tupleD vars) body)
    bind (Terminating def body) = F.let_ binding . unfoldDefinition def
      where
        binding = F.Binding (F.Symbol symbol []) body
        constant = F.constant symbol
        symbol = Typed "i" (typeOf body)

        unfoldDefinition :: [P.Var] -> F.Term -> F.Term
        unfoldDefinition vars = case NE.nonEmpty vars of
          Nothing   -> F.if_ (F.isJust constant) (F.fromJust constant)
          Just vars -> F.if_ (F.isLeft constant)
                             (F.fromLeft constant) .
                             F.let_ (F.Binding (F.tupleD vars) (F.fromRight constant))

foldFunctionBindings :: F.Term -> [FunctionBinding] -> F.Term
foldFunctionBindings = foldr bind
  where
    bind :: FunctionBinding -> F.Term -> F.Term
    bind (FunctionBinding f args body) = F.let_ (F.Binding def renaming)
      where
        def = F.Symbol f (fmap translateVar args)
        renaming = renaming' (NE.nonEmpty args)
        renaming' Nothing = body
        renaming' (Just args) = F.let_ (F.Binding def tuple) body
          where
            def   = F.tupleD (fmap translateConstant args)
            tuple = F.tupleLiteral (fmap (F.variable . translateVar) args)

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
    v  = var lval

    var (P.Variable  v)   = v
    var (P.ArrayElem v _) = v

    body = case lval of
             P.Variable  _   -> e'
             P.ArrayElem _ a -> F.store (F.constant n) (translateExpression a) e'
                                  where n = translateConstant v

    binding = Variables (v NE.:| []) body
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
      Left b -> Variables updated (F.if_ c' thenBranch elseBranch)
        where
          updated = NE.fromList undeclared
          updatedTerm = F.tupleLiteral (fmap F.constant updated)
          thenBranch = foldBindings updatedTerm thenBindings
          elseBranch = foldBindings updatedTerm elseBindings

      Right (flp, b) -> Terminating undeclared (ite (thenBranch undeclared) (elseBranch undeclared))
        where
          ite = if flp then F.if_ c' else flip (F.if_ c')

          b' = translateTerminating b

          thenBranch u = foldBindings (updates u) thenBindings
          elseBranch u = returnTerm b' u

          updates u = case NE.nonEmpty u of
            Nothing -> F.nothing (typeOf b')
            Just u  -> F.right (typeOf b') (F.tupleLiteral $ fmap F.constant u)

          returnTerm t u = case NE.nonEmpty u of
            Nothing -> F.just t
            Just u  -> F.left t (tupleType $ fmap typeOf u)


translateFunDef :: P.FunDef -> FunctionBinding
translateFunDef (P.FunDef t f vars ts) = FunctionBinding (Typed f t) vars (translateTerminating ts)

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