module Kyckling.Back where

import Data.Char
import Data.Either
import Data.List

import Kyckling.Theory
import Kyckling.FOOL
import qualified Kyckling.Program as P

translate :: P.Program -> (Signature, Term)
translate (P.Program ss []) = ([] , BooleanConst True)
translate (P.Program ss as) = (signature, conjecture)
  where
    (declared, bindings) = translateStatements ss

    bound = concatMap namesIn bindings

    signature = nub (declared \\ bound)

    conjecture = foldr Let assert bindings
    assert = foldr1 (Binary And) (map translateAssertion as)

type Declaration = Constant

namesIn :: Binding -> [Constant]
namesIn (Binding (Symbol n _) _) = [n]
namesIn (Binding (TupleD ns)  _) = ns

translateStatement :: P.Statement -> Either Declaration Binding
translateStatement (P.Declare v) = Left (translateVar v)
translateStatement (P.Assign lval e) = Right (Binding (Symbol n []) body)
  where
    e' = translateExpression e
    n  = translateVar (var lval)

    var (P.Variable  v)   = v
    var (P.ArrayElem v _) = v

    body = case lval of
             P.Variable  _   -> e'
             P.ArrayElem _ a -> FunApp Store [Const n, translateExpression a, e']
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

    constructBranch = foldr Let (FunApp Tuple $ map Const updated)

    c' = translateExpression c
    ite = If c' (constructBranch thenBindings) (constructBranch elseBindings)
    binding = Binding (TupleD updated) ite

translateStatements :: [P.Statement] -> ([Declaration], [Binding])
translateStatements = partitionEithers . map translateStatement

translateAssertion :: P.Assertion -> Term
translateAssertion (P.Assertion e) = translateExpression e

translateExpression :: P.Expression -> Term
translateExpression (P.IntegerConst i) = IntegerConst i 
translateExpression (P.BoolConst    b) = BooleanConst b
translateExpression (P.Unary op e) =
  let e' = translateExpression e in
  case op of 
    P.Negate   -> Unary Not e'
    P.Positive -> error "not supported by TPTP"
    P.Negative -> FunApp Uminus [e']
translateExpression (P.Binary op a b) =
  let a' = translateExpression a
      b' = translateExpression b in
  case op of
    P.And      -> Binary And a' b'
    P.Or       -> Binary Or  a' b'
    P.Greater  -> FunApp Greater    [a', b']
    P.Less     -> FunApp Greater    [a', b']
    P.Geq      -> FunApp Greatereq  [a', b']
    P.Leq      -> FunApp Lesseq     [a', b']
    P.Add      -> FunApp Sum        [a', b']
    P.Subtract -> FunApp Difference [a', b']
    P.Multiply -> FunApp Product    [a', b']
translateExpression (P.IfElse c a b) = If (translateExpression c) (translateExpression a) (translateExpression b)
translateExpression (P.Eql   a b) = Binary Eq   (translateExpression a) (translateExpression b)
translateExpression (P.InEql a b) = Binary InEq (translateExpression a) (translateExpression b)
translateExpression (P.Ref lval) = translateLValue lval

translateLValue :: P.LValue -> Term
translateLValue (P.Variable v)    = Const (translateVar v)
translateLValue (P.ArrayElem v e) = FunApp Select [Const (translateVar v), translateExpression e]

translateVar :: P.Var -> Constant
translateVar (P.Var v t) = (map toLower v, t)