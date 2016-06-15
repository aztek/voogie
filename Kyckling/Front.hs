{-#LANGUAGE GADTs,DataKinds #-}

module Kyckling.Front where

import Control.Monad

import Kyckling.Program
import qualified Kyckling.AST as AST

type Error = String

type Env = [(String, TType)]

emptyEnv :: Env
emptyEnv = []

lookupName :: String -> Env -> Either Error (Type t)
lookupName name env = case lookup name env of
                        Nothing        -> Left ("undefined variable " ++ name)
                        Just (TType t) -> Right t

lookupArrayName :: String -> Env -> Either Error (Type t)
lookupArrayName name env = do arr <- lookupName name env
                              case arr of
                                LiftA el -> Right undefined --el
                                otherwise -> Left "not an array"

analyze :: AST.AST -> Either Error Program
analyze (AST.AST ss as) =
  do let env = emptyEnv
     as' <- mapM (analyzeAssert env) as
     return $ Program [] as'

analyzeStmt :: Env -> AST.Stmt -> (Env, Either Error Statement)
analyzeStmt env (AST.If c a b) = undefined
analyzeStmt env (AST.Block ss) = undefined
analyzeStmt env (AST.Declare typ defs) = undefined
analyzeStmt env (AST.Increment lval) = (env, stmt)
  where stmt = do lv <- analyzeLValue env LiftI lval
                  return $ Assign lv (Binary Add (Ref lv) (IntegerConst 1))
analyzeStmt env (AST.Decrement lval) = (env, stmt)
  where stmt = do lv <- analyzeLValue env LiftI lval
                  return $ Assign lv (Binary Subtract (Ref lv) (IntegerConst 1))
analyzeStmt env (AST.Update lval op e) = (env, stmt)
  where stmt = do lv <- analyzeLValue env LiftI lval
                  e' <- analyzeExpr   env LiftI e
                  let e'' = case op of
                              AST.Assign -> e'
                              otherwise  -> undefined
                                            --let TypedBinaryOp _ _ _ op' = translateUpdateOp op
                                            -- in Binary op' (Ref lv) e'
                  return $ Assign lv e''

analyzeAssert :: Env -> AST.Assert -> Either Error Assertion
analyzeAssert env (AST.Assert e) = liftM Assertion (analyzeExpr env LiftB e)

analyzeLValue :: Env -> Type t -> AST.LVal -> Either Error (LValue t)
analyzeLValue env t lval = do { lval' <- analyzeLValue' env lval ; expect lval' t }
  where
    expect (TypedLValue t lval) t' | unliftType t == unliftType t' = undefined
    expect (TypedLValue t lval) t' = Left $ "expected an expression of the type " ++ show t' ++
                                            " but got " ++ show lval ++ " of the type " ++ show t 

analyzeLValue' :: Env -> AST.LVal -> Either Error TypedLValue
analyzeLValue' env (AST.Var name) =
  do t <- lookupName name env
     return $ TypedLValue t (Variable (Var t name))
analyzeLValue' env (AST.ArrayElem name el) =
  do el <- analyzeExpr env LiftI el
     t  <- lookupArrayName name env
     return $ TypedLValue t (ArrayElem (Var (LiftA t) name) el)


expect :: TypedExpression -> Type t -> Either Error (Expression t)
expect (TypedExpression t e) t' | unliftType t == unliftType t' = undefined
expect (TypedExpression t e) t' = Left $ "expected an expression of the type " ++ show t' ++
                                         " but got " ++ show e ++ " of the type " ++ show t 


analyzeExpr :: Env -> Type t -> AST.Expr -> Either Error (Expression t)
analyzeExpr env t e = do { e' <- analyzeExpr' env e ; expect e' t }

analyzeExpr' :: Env -> AST.Expr -> Either Error TypedExpression
analyzeExpr' _ (AST.IntConst  i) = return $ TypedExpression LiftI (IntegerConst i)
analyzeExpr' _ (AST.BoolConst b) = return $ TypedExpression LiftB (BoolConst b)
analyzeExpr' env (AST.LVal lval) =
  do TypedLValue t lv <- analyzeLValue' env lval
     return $ TypedExpression t (Ref lv)
analyzeExpr' env (AST.Prefix op e) = undefined
  --let TypedUnaryOp op' r d = translatePrefixOp op in
  --do e' <- analyzeExpr env r e
     --return $ TypedExpression d (Unary op' e')
analyzeExpr' env (AST.Infix op a b) = undefined
  --let TypedBinaryOp op' r1 r2 d = translateInfixOp op in
  --do a' <- analyzeExpr env r1 a
     --b' <- analyzeExpr env r2 b
     --return $ TypedExpression d (Binary op' a' b')
analyzeExpr' env (AST.Ternary c a b) =
  do TypedExpression t a' <- analyzeExpr' env a
     b'  <- analyzeExpr' env b
     b'' <- expect b' t
     c'  <- analyzeExpr env LiftB c
     return $ TypedExpression t (Ternary IfElse c' a' b'')


translatePrefixOp :: AST.PrefixOp -> TypedUnaryOp
translatePrefixOp op =
  case op of
    AST.Uminus -> TypedUnaryOp LiftI LiftI Negative
    AST.Uplus  -> TypedUnaryOp LiftI LiftI Positive
    AST.Not    -> TypedUnaryOp LiftB LiftB Negate

translateInfixOp :: AST.InfixOp -> TypedBinaryOp
translateInfixOp op =
  case op of
    AST.And     -> TypedBinaryOp LiftB LiftB LiftB And
    AST.Or      -> TypedBinaryOp LiftB LiftB LiftB Or
    AST.Greater -> TypedBinaryOp LiftI LiftI LiftB Greater
    AST.Less    -> TypedBinaryOp LiftI LiftI LiftB Less
    AST.Geq     -> TypedBinaryOp LiftI LiftI LiftB Geq
    AST.Leq     -> TypedBinaryOp LiftI LiftI LiftB Leq
    AST.Plus    -> TypedBinaryOp LiftI LiftI LiftI Add
    AST.Minus   -> TypedBinaryOp LiftI LiftI LiftI Subtract
    AST.Times   -> TypedBinaryOp LiftI LiftI LiftI Multiply

translateUpdateOp :: AST.UpdateOp -> TypedBinaryOp
translateUpdateOp op =
  case op of
    AST.Add      -> TypedBinaryOp LiftI LiftI LiftI Add
    AST.Subtract -> TypedBinaryOp LiftI LiftI LiftI Subtract
    AST.Multiply -> TypedBinaryOp LiftI LiftI LiftI Multiply

--translateInfixOp Select   :: BinaryOp (A b) I b
--translateInfixOp Eq       :: BinaryOp a a B
--translateInfixOp InEq     :: BinaryOp a a B