module Kyckling.Front where

import Control.Monad
import Control.Applicative

import Data.Maybe

import Kyckling.Type
import Kyckling.Program
import Kyckling.Program.Types
import Kyckling.Program.Pretty
import qualified Kyckling.Program.AST as AST

type Error = String

type Env = [(String, Type)]

emptyEnv :: Env
emptyEnv = []

lookupName :: String -> Env -> Either Error Type
lookupName name env = case lookup name env of
                        Nothing -> Left $ "undefined variable " ++ name
                        Just t  -> Right t

lookupArrayName :: String -> Env -> Either Error Type
lookupArrayName name env = do arr <- lookupName name env
                              case arr of
                                Array el  -> Right el
                                otherwise -> Left "not an array"

analyze :: AST.AST -> Either Error Program
analyze (AST.AST ss as) =
  do (env, ss') <- analyzeStmtList emptyEnv ss
     as' <- mapM (analyzeAssert env) as
     return (Program ss' as')

analyzeStmtList :: Env -> [AST.Stmt] -> Either Error (Env, [Statement])
analyzeStmtList env [] = Right (env, [])
analyzeStmtList env (s:ss) =
  do (env',  s')  <- analyzeStmt env s
     (env'', ss') <- analyzeStmtList env' ss
     return (env'', s' ++ ss')

analyzeStmt :: Env -> AST.Stmt -> Either Error (Env, [Statement])
analyzeStmt env (AST.Declare typ defs) =
  do defs' <- mapM analyzeDef defs
     let env' = map (\(n, _) -> (n, t)) defs' ++ env
     let decl = concatMap toStmts defs'
     return (env', decl)
  where
    t = translateType typ
    analyzeDef (n, e) = do e' <- mapM (analyzeExpr env t) (maybeToList e)
                           return (n, e')
    toStmts (n, e) = Declare v : map (Assign (Variable v)) e
      where v = Var n t
analyzeStmt env (AST.If c a b) =
  do (_, a') <- analyzeStmtList env a
     (_, b') <- analyzeStmtList env b
     c' <- analyzeExpr env Boolean c
     return (env, [If c' a' b'])
analyzeStmt env (AST.Increment lval) =
  do lv <- analyzeLValue env Integer lval
     return (env, [Assign lv (Binary Add (Ref lv) (IntegerConst 1))])
analyzeStmt env (AST.Decrement lval) =
  do lv <- analyzeLValue env Integer lval
     return (env, [Assign lv (Binary Subtract (Ref lv) (IntegerConst 1))])
analyzeStmt env (AST.Update lval AST.Assign e) =
  do (t, lv) <- analyzeLValue' env lval
     e' <- analyzeExpr env t e
     return (env, [Assign lv e'])
analyzeStmt env (AST.Update lval op e) =
  do lv <- analyzeLValue env d1 lval
     e' <- analyzeExpr env d2 e
     return (env, [Assign lv (Binary op' (Ref lv) e')])
  where
    op' = translateUpdateOp op
    (d1, d2) = binaryOpDomain op' -- we assume that d1 == range of op'

analyzeAssert :: Env -> AST.Assert -> Either Error Assertion
analyzeAssert env (AST.Assert e) = Assertion <$> analyzeExpr env Boolean e

analyzeLValue :: Env -> Type -> AST.LVal -> Either Error LValue
analyzeLValue env t lval = do (t', lval') <- analyzeLValue' env lval
                              if t == t' then return lval' else
                                Left $ "expected an expression of the type " ++ prettyType t' ++
                                       " but got " ++ prettyLValue lval' ++ " of the type " ++ prettyType t 

analyzeLValue' :: Env -> AST.LVal -> Either Error (Type, LValue)
analyzeLValue' env (AST.Var name) =
  do t <- lookupName name env
     return (t, Variable (Var name t))
analyzeLValue' env (AST.ArrayElem name el) =
  do el <- analyzeExpr env Integer el
     t  <- lookupArrayName name env
     return (t, ArrayElem (Var name t) el)

analyzeExpr :: Env -> Type -> AST.Expr -> Either Error Expression
analyzeExpr env t e = do (t', e') <- analyzeExpr' env e
                         if t == t' then return e' else
                           Left $ "expected an expression of the type " ++ prettyType t' ++
                                  " but got " ++ prettyExpression e' ++ " of the type " ++ prettyType t 

analyzeExpr' :: Env -> AST.Expr -> Either Error (Type, Expression)
analyzeExpr' _ (AST.IntConst  i) = return (Integer, IntegerConst i)
analyzeExpr' _ (AST.BoolConst b) = return (Boolean, BoolConst b)
analyzeExpr' env (AST.LVal lval) =
  do (t, lv) <- analyzeLValue' env lval
     return (t, Ref lv)
analyzeExpr' env (AST.Prefix op e) =
  do e' <- analyzeExpr env d e
     return (r, Unary op' e')
  where
    op' = translatePrefixOp op
    r   = unaryOpRange op'
    d   = unaryOpDomain op'
analyzeExpr' env (AST.Infix AST.Eq a b) =
  do (t, a') <- analyzeExpr' env a
     b' <- analyzeExpr env t b
     return (Boolean, Eql a' b')
analyzeExpr' env (AST.Infix AST.NonEq a b) =
  do (t, a') <- analyzeExpr' env a
     b' <- analyzeExpr env t b
     return (Boolean, InEql a' b')
analyzeExpr' env (AST.Infix op a b) =
  do a' <- analyzeExpr env d1 a
     b' <- analyzeExpr env d2 b
     return (r, Binary op' a' b')
  where
    op' = translateInfixOp op
    (d1, d2) = binaryOpDomain op'
    r = binaryOpRange op'
analyzeExpr' env (AST.Ternary c a b) =
  do c' <- analyzeExpr env Boolean c
     (t, a') <- analyzeExpr' env a
     b' <- analyzeExpr env t b
     return (t, Ternary IfElse c' a' b')


translatePrefixOp :: AST.PrefixOp -> UnaryOp
translatePrefixOp op =
  case op of
    AST.Uminus -> Negative
    AST.Uplus  -> Positive
    AST.Not    -> Negate

translateInfixOp :: AST.InfixOp -> BinaryOp
translateInfixOp op =
  case op of
    AST.And     -> And
    AST.Or      -> Or
    AST.Greater -> Greater
    AST.Less    -> Less
    AST.Geq     -> Geq
    AST.Leq     -> Leq
    AST.Plus    -> Add
    AST.Minus   -> Subtract
    AST.Times   -> Multiply

translateUpdateOp :: AST.UpdateOp -> BinaryOp
translateUpdateOp op =
  case op of
    AST.Add      -> Add
    AST.Subtract -> Subtract
    AST.Multiply -> Multiply

translateType :: AST.Type -> Type
translateType AST.I = Integer
translateType AST.B = Boolean
translateType (AST.Array t) = Array (translateType t)