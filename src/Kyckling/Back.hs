module Kyckling.Back where

import Data.Char
import Data.Either
import Data.List
import qualified Data.Set as S
import Data.Set (Set)

import qualified Data.List.NonEmpty as NE

import Kyckling.Theory
import qualified Kyckling.FOOL as F
import qualified Kyckling.FOOL.Tuple as F.Tuple

import qualified Kyckling.Program as P

data Binding = Regular F.Definition F.Term
             | MaybeBinding F.Term
             | EitherBinding F.Definition F.Term

namesIn :: Binding -> [F.Const]
namesIn (Regular def _) = namesInDefinition def
namesIn (MaybeBinding _) = []
namesIn (EitherBinding def _) = namesInDefinition def

namesInDefinition :: F.Definition -> [F.Const]
namesInDefinition (F.Symbol c _) = [c]
namesInDefinition (F.TupleD cs)  = F.Tuple.toList cs

translate :: P.Program -> (Signature, F.Formula)
translate (P.Program fs ss as) = case NE.nonEmpty as of
  Nothing -> (S.empty, F.BooleanConstant True)
  Just as -> (S.fromList signature, conjecture)
    where
      funBindings = map translateFunDef fs
      (declared, bindings) = translateStatements ss

      bound = concatMap namesIn bindings
      nonArrays = filter (not . isArray . typeOf)

      signature = nub (declared \\ nonArrays bound)

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
        constant = F.Constant symbol
    f (EitherBinding def b) = F.Let eitherBinding . F.If (F.IsLeft constant) (F.FromLeft constant) . F.Let defBinding
      where
        eitherBinding = F.Binding (F.Symbol symbol []) b
        symbol = Typed "i" (typeOf b)
        constant = F.Constant symbol

        defBinding = F.Binding def (F.FromRight constant)

type Declaration = F.Const

translateStatements :: [P.Statement] -> ([Declaration], [Binding])
translateStatements = partitionEithers . map translateStatement

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
             P.ArrayElem _ a -> F.Store (F.Constant n) (translateExpression a) e'

    binding = Regular (F.Symbol n []) body
translateStatement (P.If c a b) = Right binding
  where
    (thenDeclared, thenBindings) = translateStatements a
    (elseDeclared, elseBindings) = translateStatements b

    thenBound = concatMap namesIn thenBindings
    elseBound = concatMap namesIn elseBindings
    bound = nub (thenBound ++ elseBound)

    declared = nub (thenDeclared ++ elseDeclared)

    -- TODO: for now we assume that there are no unbound declarations;
    --       this assumption must be removed in the future

    ite = F.If (translateExpression c)

    (def, updatedTerm) = case NE.nonEmpty (bound \\ declared) of
      Nothing -> error "invariant violation" -- TODO: can it really never happen?
      Just updated ->
        case F.Tuple.nonUnit updated of
          Left var      -> (F.Symbol var [],  F.Constant var)
          Right updated -> (F.TupleD updated, F.TupleLiteral (fmap F.Constant updated))

    thenBranch = foldBindings updatedTerm thenBindings
    elseBranch = foldBindings updatedTerm elseBindings
    body = ite thenBranch elseBranch
    binding = Regular def body

translateStatement (P.IfTerminating c flp a b) = Right binding
  where
    (thenDeclared, thenBindings) = translateStatements a
    elseTerm = translateTerminating b

    bound = nub (concatMap namesIn thenBindings)
    declared = nub thenDeclared

    ite = if flp then f else flip f 
      where f = F.If (translateExpression c)

    binding = case NE.nonEmpty (bound \\ declared) of
      Nothing -> MaybeBinding body
        where
          thenBranch = foldBindings (F.Nothing_ (typeOf elseTerm)) thenBindings
          elseBranch = F.Just_ elseTerm
          body = ite thenBranch elseBranch

      Just updated -> EitherBinding def body
        where
          (def, updatedTerm) = case F.Tuple.nonUnit updated of
            Left var      -> (F.Symbol var [],  F.Constant var)
            Right updated -> (F.TupleD updated, F.TupleLiteral (fmap F.Constant updated))
          thenBranch = foldBindings (F.Right_ (typeOf elseTerm) updatedTerm) thenBindings
          elseBranch = F.Left_ elseTerm (typeOf updatedTerm)
          body = ite thenBranch elseBranch

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
translateExpression (P.Ref lval)      = translateLValue lval

translateLValue :: P.LValue -> F.Term
translateLValue (P.Variable v)    = F.Constant (translateVar v)
translateLValue (P.ArrayElem v e) = F.Select (F.Constant (translateVar v)) (translateExpression e)

translateVar :: P.Var -> F.Const
translateVar (Typed v t) = Typed (map toLower v) t