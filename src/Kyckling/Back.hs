module Kyckling.Back where

import Data.Char
import Data.Either
import Data.List

import Kyckling.Theory
import qualified Kyckling.FOOL as F
import qualified Kyckling.Program as P

translate :: P.Program -> (Signature, F.Formula)
translate (P.Program fs ss []) = ([] , F.BooleanConst True)
translate (P.Program fs ss as) = (signature, conjecture)
  where
    (declared, bindings) = translateStatements ss

    bound = concatMap namesIn bindings
    nonArrays = filter (\(Typed _ t) -> not $ isArray t)

    signature = nub (declared \\ nonArrays bound)

    conjecture = foldr F.Let assert bindings
    assert = foldr1 (F.Binary And) (map (\(P.Assertion f) -> f) as)

type Declaration = F.Constant

namesIn :: F.Binding -> [F.Constant]
namesIn (F.Binding (F.Symbol c _) _) = [c]
namesIn (F.Binding (F.TupleD cs)  _) = cs

translateStatement :: P.Statement -> Either Declaration F.Binding
translateStatement (P.Declare v) = Left (translateVar v)
translateStatement (P.Assign lval e) = Right (F.Binding (F.Symbol n []) body)
  where
    e' = translateExpression e
    n  = translateVar (var lval)

    var (P.Variable  v)   = v
    var (P.ArrayElem v _) = v

    body = case lval of
             P.Variable  _   -> e'
             P.ArrayElem _ a -> F.Store (F.Const n) (translateExpression a) e'
translateStatement (P.If c a b) = Right binding
  where
    (thenDeclared, thenBindings) = translateStatements a
    (elseDeclared, elseBindings) = translateStatements b

    thenBound = concatMap namesIn thenBindings
    elseBound = concatMap namesIn elseBindings
    bound = nub (thenBound ++ elseBound)

    declared = nub (thenDeclared ++ elseDeclared)

    updated = bound \\ declared

    -- TODO: for now we assume that there are no unbound declarations;
    --       this assumption must be removed in the future

    constructBranch = foldr F.Let (F.Tuple $ map F.Const updated)

    c' = translateExpression c
    ite = F.If c' (constructBranch thenBindings) (constructBranch elseBindings)
    binding = F.Binding (F.TupleD updated) ite
translateStatement (P.IfTerminating{}) = undefined

translateStatements :: [P.Statement] -> ([Declaration], [F.Binding])
translateStatements = partitionEithers . map translateStatement

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