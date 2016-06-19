module Kyckling.Back where

import Data.Char
import Data.Either
import Data.List

import Kyckling.TPTP
import qualified Kyckling.Program as P

translate :: P.Program -> TPTP
translate (P.Program ss as) = TPTP sortDecls conjecture
  where
    (ds, bs) = translateStatements ss

    sortDecls = map (uncurry SortDeclaration) ds

    conjecture = Conjecture "1" boundAssert
    boundAssert = foldr Let assert bs
    assert = foldr (Binary And . translateAssertion) (BooleanConst False) as

type Declaration = (String, Sort)

deleteByKey :: String -> [Declaration] -> [Declaration]
deleteByKey n = filter ((n /=) . fst)

deleteN :: [String] -> [Declaration] -> [Declaration]
deleteN = flip (foldr deleteByKey)

translateStatement :: P.Statement -> Either Declaration Binding
translateStatement (P.Declare v Nothing) = Left (translateVar v)
translateStatement (P.Declare v (Just e)) = undefined
translateStatement (P.Assign lval e) = Right (Binding (Symbol n []) body)
  where
    e' = translateExpression e
    n  = translateVarName (var lval)

    var (P.Variable  v)   = v
    var (P.ArrayElem v _) = v

    body = case lval of
             P.Variable  _   -> e'
             P.ArrayElem _ a -> FunApp Store [Constant n, translateExpression a, e']

translateStatement (P.If c a b) = Right binding
  where
    (_, a') = translateStatements a
    (_, b') = translateStatements b

    updated = nub (concatMap namesIn a' `union` concatMap namesIn b')

    constructBranch = foldr Let (FunApp Tuple $ map Constant updated)

    c' = translateExpression c
    ite = If c' (constructBranch a') (constructBranch b')
    binding = Binding (TupleD updated) ite

translateStatements :: [P.Statement] -> ([Declaration], [Binding])
translateStatements = define . partitionEithers . map translateStatement
  where
    define (ds, bs) = (deleteN (concatMap namesIn bs) ds, bs)

namesIn :: Binding -> [Constant]
namesIn (Binding (Symbol n _) _) = [n]
namesIn (Binding (TupleD ns)  _) = ns

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
translateExpression (P.Ternary P.IfElse c a b) = If (translateExpression c) (translateExpression a) (translateExpression b)
translateExpression (P.Eql   a b) = Binary Eq   (translateExpression a) (translateExpression b)
translateExpression (P.InEql a b) = Binary InEq (translateExpression a) (translateExpression b)
translateExpression (P.Ref lval) = translateLValue lval

translateLValue :: P.LValue -> Term
translateLValue (P.Variable v)    = Constant (translateVarName v)
translateLValue (P.ArrayElem v e) = FunApp Select [Constant (translateVarName v), translateExpression e]

translateVarName :: P.Var -> Constant
translateVarName (P.Var v _) = map toLower v

translateVar :: P.Var -> (Constant, Sort)
translateVar v@(P.Var _ t) = (translateVarName v, translateType t)

translateType :: P.Type -> Sort
translateType P.Integer = Integer
translateType P.Boolean = Boolean
translateType (P.Array t) = Array Integer (translateType t)