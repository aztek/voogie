module Kyckling.Front where

import Control.Monad
import Control.Applicative
import Data.Maybe

import qualified Data.Map as Map

import Kyckling.Theory
import Kyckling.Pretty
import Kyckling.Program
import Kyckling.Program.Pretty
import qualified Kyckling.Program.AST as AST

import qualified Kyckling.FOOL as F
import qualified Kyckling.FOOL.AST as F.AST
import qualified Kyckling.FOOL.Pretty as F.P

type Error = String

type Env = Map.Map String Type

emptyEnv :: Env
emptyEnv = Map.empty

lookupName :: String -> Env -> Either Error (Typed Name)
lookupName name env = case Map.lookup name env of
                        Nothing  -> Left  ("undefined variable " ++ name)
                        Just typ -> Right (Typed name typ)

lookupArrayName :: String -> Env -> Either Error (Typed Name)
lookupArrayName name env = do name <- lookupName name env
                              case typeOf name of
                                Array _   -> Right name
                                otherwise -> Left "not an array"

guardType :: (TypeOf b, Pretty b) => Type -> (a -> Either Error b) -> a -> Either Error b
guardType t analyze a = do b <- analyze a
                           let t' = typeOf b
                           if t == t' then return b else
                             Left $ "expected an expression of the type " ++ pretty t' ++
                                    " but got " ++ pretty b ++ " of the type " ++ pretty t 

analyze :: AST.AST -> Either Error Program
analyze (AST.AST fs ss as) =
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
analyzeStmt env (AST.Declare t defs) =
  do defs' <- mapM analyzeDef defs
     let env' = foldr (\(n, _) -> Map.insert n t) env defs'
     let decl = concatMap toStmts defs'
     return (env', decl)
  where
    analyzeDef (n, e) = do e' <- mapM (guardType t (analyzeExpr env)) (maybeToList e)
                           return (n, e')
    toStmts (n, e) = Declare v : map (Assign (Variable v)) e
      where v = Typed n t
analyzeStmt env (AST.If c a b) =
  do (_, a') <- analyzeStmtList env a
     (_, b') <- analyzeStmtList env b
     c' <- guardType Boolean (analyzeExpr env) c
     return (env, [If c' a' b'])
analyzeStmt env (AST.Increment lval) =
  do lv <- guardType Integer (analyzeLValue env) lval
     return (env, [Assign lv (Binary Add (Ref lv) (IntegerConst 1))])
analyzeStmt env (AST.Decrement lval) =
  do lv <- guardType Integer (analyzeLValue env) lval
     return (env, [Assign lv (Binary Subtract (Ref lv) (IntegerConst 1))])
analyzeStmt env (AST.Update lval AST.Assign e) =
  do lv <- analyzeLValue env lval
     e' <- guardType (typeOf lv) (analyzeExpr env) e
     return (env, [Assign lv e'])
analyzeStmt env (AST.Update lval op e) =
  do lv <- guardType d1 (analyzeLValue env) lval
     e' <- guardType d2 (analyzeExpr env) e
     return (env, [Assign lv (Binary op' (Ref lv) e')])
  where
    op' = case op of
            AST.Plus  -> Add
            AST.Minus -> Subtract
            AST.Times -> Multiply
            AST.Assign -> undefined -- covered by the previous case
    (d1, d2) = binaryOpDomain op' -- we assume that d1 == range of op'
analyzeStmt env _ = undefined

analyzeAssert :: Env -> AST.Assert -> Either Error Assertion
analyzeAssert env (AST.Assert f) = Assertion <$> analyzeFormula env f

analyzeFormula :: Env -> F.AST.Term -> Either Error F.Formula
analyzeFormula env = guardType Boolean (analyzeTerm env)

analyzeTerm :: Env -> F.AST.Term -> Either Error F.Term
analyzeTerm _ (F.AST.IntConst  i) = return (F.IntegerConst i)
analyzeTerm _ (F.AST.BoolConst b) = return (F.BooleanConst b)
analyzeTerm env (F.AST.Unary op t) = F.Unary op <$> guardType d (analyzeTerm env) t
  where
    d = unaryOpDomain op
analyzeTerm env (F.AST.Binary op a b) = F.Binary op <$> guardType d1 (analyzeTerm env) a <*> guardType d2 (analyzeTerm env) b
  where
    (d1, d2) = binaryOpDomain op
analyzeTerm env (F.AST.Ternary c a b) = 
  do c' <- analyzeFormula env c
     a' <- analyzeTerm env a
     b' <- guardType (typeOf a') (analyzeTerm env) b
     return (F.If c' a' b')
analyzeTerm env (F.AST.Eql a b) =
  do a' <- analyzeTerm env a
     b' <- guardType (typeOf a') (analyzeTerm env) b
     return (F.Eql a' b')
analyzeTerm env (F.AST.InEql a b) =
  do a' <- analyzeTerm env a
     b' <- guardType (typeOf a') (analyzeTerm env) b
     return (F.InEql a' b')
analyzeTerm env (F.AST.Quantified q vars term) = F.Quantify q vars' <$> analyzeFormula env' term
  where
    -- TODO: check that the variables are disjoint
    env' = foldr (\(Typed v t) -> Map.insert v t) env vars
    vars' = map (fmap F.Var) vars
analyzeTerm env (F.AST.Constant  s)   = F.Const  <$> lookupName s env
analyzeTerm env (F.AST.ArrayElem s i) = F.Select <$> (F.Const <$> lookupArrayName s env) <*> guardType Integer (analyzeTerm env) i


analyzeLValue :: Env -> AST.LVal -> Either Error LValue
analyzeLValue env (AST.Var s) = Variable <$> lookupName s env
analyzeLValue env (AST.ArrayElem s i) = ArrayElem <$> lookupArrayName s env <*> guardType Integer (analyzeExpr env) i


analyzeExpr :: Env -> AST.Expr -> Either Error Expression
analyzeExpr _ (AST.IntConst  i) = return (IntegerConst i)
analyzeExpr _ (AST.BoolConst b) = return (BooleanConst b)
analyzeExpr env (AST.LVal lval) = Ref <$> analyzeLValue env lval
analyzeExpr env (AST.Unary op e) = Unary op <$> guardType d (analyzeExpr env) e
  where
    d = unaryOpDomain op
analyzeExpr env (AST.Binary op a b) =
  do a' <- guardType d1 (analyzeExpr env) a
     b' <- guardType d2 (analyzeExpr env) b
     return (Binary op a' b')
  where
    (d1, d2) = binaryOpDomain op
analyzeExpr env (AST.Eql a b) =
  do a' <- analyzeExpr env a
     b' <- guardType (typeOf a') (analyzeExpr env) b
     return (Eql a' b')
analyzeExpr env (AST.InEql a b) =
  do a' <- analyzeExpr env a
     b' <- guardType (typeOf a') (analyzeExpr env) b
     return (InEql a' b')
analyzeExpr env (AST.Ternary c a b) =
  do c' <- guardType Boolean (analyzeExpr env) c
     a' <- analyzeExpr env a
     b' <- guardType (typeOf a') (analyzeExpr env) b
     return (IfElse c' a' b')