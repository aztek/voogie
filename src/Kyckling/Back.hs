module Kyckling.Back where

import Data.Char
import Data.Either
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Kyckling.Theory
import qualified Kyckling.FOOL as F
import qualified Kyckling.Program as P

data Binding = Regular F.Binding
             | MaybeBinding F.Term
             | EitherBinding (NonEmpty F.Constant) F.Term

namesIn :: Binding -> [F.Constant]
namesIn (Regular (F.Binding (F.Symbol c _) _)) = [c]
namesIn (Regular (F.Binding (F.TupleD cs)  _)) = NE.toList cs
namesIn (MaybeBinding _) = []
namesIn (EitherBinding cs _) = NE.toList cs

translate :: P.Program -> (Signature, F.Formula)
translate (P.Program _  ss []) = ([] , F.BooleanConst True)
translate (P.Program fs ss as) = (signature, conjecture)
  where
    funBindings = map translateFunDef fs
    (declared, bindings) = translateStatements ss

    bound = concatMap namesIn bindings
    nonArrays = filter (\(Typed _ t) -> not $ isArray t)

    signature = nub (declared \\ nonArrays bound)

    assert = foldr1 (F.Binary And) (map (\(P.Assertion f) -> f) as)
    conjecture = foldBindings assert (funBindings ++ bindings)

foldBindings :: F.Term -> [Binding] -> F.Term
foldBindings = foldr f
  where
    f (Regular b) t = F.Let b t
    f (MaybeBinding b) t = F.Let binding (F.If (F.IsJust constant) (F.FromJust constant) t)
      where
        binding = F.Binding (F.Symbol symbol []) b
        symbol = Typed "i" (typeOf b)
        constant = F.Const symbol
    f (EitherBinding vars b) t = F.Let binding (F.If (F.IsLeft constant)
                                                     (F.FromLeft constant)
                                                     (F.Let (F.Binding (F.TupleD vars) (F.FromRight constant)) t))
      where
        binding = F.Binding (F.Symbol symbol []) b
        symbol = Typed "i" (typeOf b)
        constant = F.Const symbol

type Declaration = F.Constant

translateStatements :: [P.Statement] -> ([Declaration], [Binding])
translateStatements = partitionEithers . map translateStatement

translateStatement :: P.Statement -> Either Declaration Binding
translateStatement (P.Declare v) = Left (translateVar v)
translateStatement (P.Assign lval e) = Right (Regular binding)
  where
    e' = translateExpression e
    n  = translateVar (var lval)

    var (P.Variable  v)   = v
    var (P.ArrayElem v _) = v

    body = case lval of
             P.Variable  _   -> e'
             P.ArrayElem _ a -> F.Store (F.Const n) (translateExpression a) e'

    binding = F.Binding (F.Symbol n []) body
translateStatement (P.If c a b) = Right (Regular binding)
  where
    (thenDeclared, thenBindings) = translateStatements a
    (elseDeclared, elseBindings) = translateStatements b

    thenBound = concatMap namesIn thenBindings
    elseBound = concatMap namesIn elseBindings
    bound = nub (thenBound ++ elseBound)

    declared = nub (thenDeclared ++ elseDeclared)

    updated = NE.fromList (bound \\ declared) -- TODO: NEED A CHECK HERE

    -- TODO: for now we assume that there are no unbound declarations;
    --       this assumption must be removed in the future

    updatedTerm = F.Tuple (NE.map F.Const updated)

    c' = translateExpression c
    thenBranch = foldBindings updatedTerm thenBindings
    elseBranch = foldBindings updatedTerm elseBindings
    ite = F.If c' thenBranch elseBranch
    binding = F.Binding (F.TupleD updated) ite
translateStatement (P.IfTerminating c flp a b) = Right binding
  where
    c' = translateExpression c
    (thenDeclared, thenBindings) = translateStatements a
    elseTerm = translateTerminating b

    bound = nub (concatMap namesIn thenBindings)
    declared = nub thenDeclared

    binding = case NE.nonEmpty (bound \\ declared) of
      Nothing -> MaybeBinding body
        where
          thenBranch = foldBindings (F.Nothing_ (typeOf elseTerm)) thenBindings
          elseBranch = F.Just_ elseTerm

          ite = if flp then F.If c' else flip (F.If c') 
          body = ite thenBranch elseBranch

      Just updated -> EitherBinding updated body
        where
          updatedTerm = F.Tuple (NE.map F.Const updated)

          thenBranch = foldBindings (F.Right_ (typeOf elseTerm) updatedTerm) thenBindings
          elseBranch = F.Left_ elseTerm (typeOf updatedTerm)

          ite = if flp then F.If c' else flip (F.If c') 
          body = ite thenBranch elseBranch

translateFunDef :: P.FunDef -> Binding
translateFunDef (P.FunDef t f vars ts) = Regular binding
  where
    symbol = F.Symbol (Typed f t) vars'
    vars' = map (fmap F.Var) vars
    binding = F.Binding symbol (translateTerminating ts)

translateTerminating :: P.TerminatingStatement -> F.Term
translateTerminating (P.Return    ss e)     = foldBindings (translateExpression e) (snd $ translateStatements ss)
translateTerminating (P.IteReturn ss c a b) = foldBindings (F.If c' a' b')         (snd $ translateStatements ss)
  where
    a' = translateTerminating a
    b' = translateTerminating b
    c' = translateExpression c

translateExpression :: P.Expression -> F.Term
translateExpression (P.IntegerConst i) = F.IntegerConst i 
translateExpression (P.BooleanConst b) = F.BooleanConst b
translateExpression (P.Unary  op e)    = F.Unary  op (translateExpression e)
translateExpression (P.Binary op a b)  = F.Binary op (translateExpression a) (translateExpression b)
translateExpression (P.IfElse c a b)   = F.If (translateExpression c) (translateExpression a) (translateExpression b)
translateExpression (P.Eql   a b)      = F.Eql   (translateExpression a) (translateExpression b)
translateExpression (P.InEql a b)      = F.InEql (translateExpression a) (translateExpression b)
translateExpression (P.Ref lval)       = translateLValue lval

translateLValue :: P.LValue -> F.Term
translateLValue (P.Variable v)    = F.Const (translateVar v)
translateLValue (P.ArrayElem v e) = F.Select (F.Const (translateVar v)) (translateExpression e)

translateVar :: P.Var -> F.Constant
translateVar (Typed v t) = Typed (map toLower v) t