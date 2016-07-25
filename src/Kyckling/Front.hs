module Kyckling.Front where

import Control.Monad (zipWithM)
import Control.Applicative
import Data.Maybe
import Data.Bifunctor

import qualified Data.Map as Map
import Data.Map (Map)

import Kyckling.Theory
import Kyckling.Pretty
import Kyckling.Program
import Kyckling.Program.Pretty
import qualified Kyckling.Program.AST as AST

import qualified Kyckling.FOOL.Smart as F
import qualified Kyckling.FOOL.AST as F.AST
import qualified Kyckling.FOOL.Pretty as F.P

type Error = String

data FunType = FunType [Type] Type

data Env = Env (Map Name FunType) (Map Name Type)

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

lookupVariable :: Name -> Env -> Either Error (Typed Name)
lookupVariable name (Env _ vs) = case Map.lookup name vs of
                                   Nothing  -> Left  ("undefined variable " ++ name)
                                   Just typ -> Right (Typed name typ)

lookupArrayName :: Name -> Env -> Either Error (Typed Name)
lookupArrayName name env = do name <- lookupVariable name env
                              case typeOf name of
                                Array _   -> Right name
                                otherwise -> Left "not an array"

lookupFunction :: Name -> Env -> Either Error FunType
lookupFunction name (Env fs _) = case Map.lookup name fs of
                                   Nothing  -> Left ("undefined function " ++ name)
                                   Just typ -> Right typ

insertVariable :: Typed Name -> Env -> Env
insertVariable (Typed n t) (Env fs vs) = Env fs (Map.insert n t vs)

insertFunction :: (Name, FunType) -> Env -> Env
insertFunction (n, t) (Env fs vs) = Env (Map.insert n t fs) vs


analyze :: AST.AST -> Either Error Program
analyze (AST.AST fs ss as) =
  do (env,  fs') <- analyzeFunDefs emptyEnv fs
     (env', ss') <- analyzeNonTerminating env ss
     as' <- mapM (analyzeAssert env') as
     return (Program fs' ss' as')

analyzeFunDefs :: Env -> [AST.FunDef] -> Either Error (Env, [FunDef])
analyzeFunDefs env [] = Right (env, [])
analyzeFunDefs env (f:fs) =
  do (env',  f')  <- analyzeFunDef  env  f
     (env'', fs') <- analyzeFunDefs env' fs
     return (env'', f':fs')

analyzeFunDef :: Env -> AST.FunDef -> Either Error (Env, FunDef)
analyzeFunDef env (AST.FunDef t n args stmts) =
  do let env' = foldr insertVariable env args
     (_, ts) <- analyzeTerminating env' stmts
     let t' = typeOf ts 
     if t == t'
     then return (insertFunction (n, FunType (map typeOf args) t) env, FunDef t n args ts)
     else Left $ "function " ++ n ++ " returns a value of the type " ++ pretty t' ++
                 " while it is declared to return " ++ pretty t

analyzeTerminating :: Env -> [AST.Stmt] -> Either Error (Env, Terminating)
analyzeTerminating env ss =
  do (env', ss') <- analyzeStmts env ss
     case ss' of
       Left  ts  -> Right (env', ts)
       Right ss' -> Left "non-terminating statement in a terminating block"

analyzeNonTerminating :: Env -> [AST.Stmt] -> Either Error (Env, NonTerminating)
analyzeNonTerminating env ss =
  do (env', ss') <- analyzeStmts env ss
     case ss' of
       Left  ts  -> Left "terminating statement in a non-terminating block"
       Right ss' -> return (env', ss')


analyzeStmts :: Env -> [AST.Stmt] -> Either Error (Env, Either Terminating NonTerminating)
analyzeStmts env = analyzeStmts' env . flattenDeclarations
  where
    flattenDeclarations :: [AST.Stmt] -> [AST.Stmt]
    flattenDeclarations = concatMap flattenDeclaration

    flattenDeclaration :: AST.Stmt -> [AST.Stmt]
    flattenDeclaration (AST.Declare t defs) = concatMap toStmts defs
      where
        toStmts (n, e) = AST.Declare t [(n, Nothing)] : maybeToList (fmap (AST.Update (AST.Var n) AST.Assign) e)
    flattenDeclaration (AST.If c a b) = [AST.If c (flattenDeclarations a) (flattenDeclarations b)]
    flattenDeclaration s = [s]

    analyzeStmts' :: Env -> [AST.Stmt] -> Either Error (Env, Either Terminating NonTerminating)
    analyzeStmts' env [] = Right (env, Right [])
    analyzeStmts' env (s:ss) =
      do (env',  s')  <- analyzeStmt env s
         case s' of
           Left ts  -> return (env', Left ts)
           Right s' -> do (env'', ss') <- analyzeStmts' env' ss
                          return (env'', bimap (appendTerminating s') (appendNonTerminating s') ss')
      where
        appendTerminating :: Statement -> Terminating -> Terminating
        appendTerminating s (Terminating nt r) = Terminating (appendNonTerminating s nt) r

        appendNonTerminating :: Statement -> NonTerminating -> NonTerminating
        appendNonTerminating = (:)


guardType :: (TypeOf b, Pretty b) => (a -> Either Error b) -> Type -> a -> Either Error b
guardType analyze t a = do b <- analyze a
                           let t' = typeOf b
                           if t == t' then return b else
                             Left $ "expected an expression of the type " ++ pretty t' ++
                                    " but got " ++ pretty b ++ " of the type " ++ pretty t 

guardTypes :: (TypeOf b, Pretty b) => (a -> Either Error b) -> [Type] -> [a] -> Either Error [b]
guardTypes analyze = zipWithM (guardType analyze)

infix 6 `guard`
guard :: (TypeOf b, Pretty b) => (a -> Either Error b) -> a -> Type -> Either Error b
guard f = flip (guardType f)

infix 6 `guardAll`
guardAll :: (TypeOf b, Pretty b) => (a -> Either Error b) -> [a] -> [Type] -> Either Error [b]
guardAll f = flip (guardTypes f)

infix 5 .:
(.:) :: (a -> b) -> a -> b
(.:) = ($)


analyzeStmt :: Env -> AST.Stmt -> Either Error (Env, Either Terminating Statement)
analyzeStmt env (AST.Declare t [(n, Nothing)]) =
  case lookupVariable n env of
    Left  _ -> Right (insertVariable var env, Right $ Declare var) where var = Typed n t
    Right _ -> Left  ("the variable " ++ n ++ " shadows the previous definition")
analyzeStmt env (AST.Declare t _) = error "should be eliminated by flattenDeclaration"
analyzeStmt env (AST.If c a b) =
  do (_, a') <- analyzeStmts env a
     (_, b') <- analyzeStmts env b
     c' <- analyzeExpr env `guard` c .: Boolean
     let stmt = case (a', b') of
                  (Right a', Right b') -> Right $ If c' a' (Left b')
                  (Right a', Left  b') -> Right $ If c' a' (Right (False, b'))
                  (Left  a', Right b') -> Right $ If c' b' (Right (True,  a'))
                  (Left  a', Left  b') -> Left  $ Terminating [] (IteReturn c' a' b')
     return (env, stmt)
analyzeStmt env (AST.Increment lval) =
  do lv <- analyzeLValue env `guard` lval .: Integer
     let stmt = Assign lv (Binary Add (Ref lv) (IntegerLiteral 1))
     return (env, Right stmt)
analyzeStmt env (AST.Decrement lval) =
  do lv <- analyzeLValue env `guard` lval .: Integer
     let stmt = Assign lv (Binary Subtract (Ref lv) (IntegerLiteral 1))
     return (env, Right stmt)
analyzeStmt env (AST.Update lval AST.Assign e) =
  do lv <- analyzeLValue env lval
     e' <- analyzeExpr env `guard` e .: typeOf lv
     let stmt = Assign lv e'
     return (env, Right stmt)
analyzeStmt env (AST.Update lval op e) =
  do lv <- analyzeLValue env `guard` lval .: d1
     e' <- analyzeExpr   env `guard` e    .: d2
     let stmt = Assign lv (Binary op' (Ref lv) e')
     return (env, Right stmt)
  where
    op' = case op of
            AST.Plus  -> Add
            AST.Minus -> Subtract
            AST.Times -> Multiply
            AST.Assign -> error "covered by a case of analyzeStmt"
    (d1, d2) = binaryOpDomain op' -- we assume that d1 == range of op'
analyzeStmt env (AST.Return e) =
  do e' <- analyzeExpr env e
     let stmt = Terminating [] (Return e')
     return (env, Left stmt)

analyzeAssert :: Env -> AST.Assert -> Either Error Assertion
analyzeAssert env (AST.Assert f) = Assertion <$> analyzeFormula env f

analyzeFormula :: Env -> F.AST.Term -> Either Error F.Formula
analyzeFormula env f = analyzeTerm env `guard` f .: Boolean

analyzeTerm :: Env -> F.AST.Term -> Either Error F.Term
analyzeTerm _ (F.AST.IntConst  i) = return (F.integerConstant i)
analyzeTerm _ (F.AST.BoolConst b) = return (F.booleanConstant b)
analyzeTerm env (F.AST.Const c) = F.constant <$> lookupVariable c env
analyzeTerm env (F.AST.Unary op t) = F.unary op <$> analyzeTerm env `guard` t .: d
  where
    d = unaryOpDomain op
analyzeTerm env (F.AST.Binary op a b) = F.binary op <$> a' <*> b'
  where
    a' = analyzeTerm env `guard` a .: d1
    b' = analyzeTerm env `guard` b .: d2
    (d1, d2) = binaryOpDomain op
analyzeTerm env (F.AST.Ternary c a b) = 
  do c' <- analyzeFormula env c
     a' <- analyzeTerm env a
     b' <- analyzeTerm env `guard` b .: typeOf a'
     return (F.if_ c' a' b')
analyzeTerm env (F.AST.Equals s a b) =
  do a' <- analyzeTerm env a
     b' <- analyzeTerm env `guard` b .: typeOf a'
     return (F.equals s a' b')
analyzeTerm env (F.AST.Quantified q vars term) = F.quantify q vars' <$> analyzeFormula env' term
  where
    -- TODO: check that the variables are disjoint
    env' = foldr insertVariable env vars
    vars' = map (fmap F.Var) vars
analyzeTerm env (F.AST.FunApp f args) =
  do FunType ts r <- lookupFunction f env
     args' <- analyzeTerm env `guardAll` args .: ts
     return (F.application (Typed f r) args')
analyzeTerm env (F.AST.ArrayElem s i) = F.select <$> array <*> index
  where
    array = F.constant <$> lookupArrayName s env
    index = analyzeTerm env `guard` i .: Integer

analyzeLValue :: Env -> AST.LVal -> Either Error LValue
analyzeLValue env (AST.Var s) = Variable <$> lookupVariable s env
analyzeLValue env (AST.ArrayElem s i) = ArrayElem <$> array <*> index
  where
    array = lookupArrayName s env
    index = analyzeExpr env `guard` i .: Integer

analyzeExpr :: Env -> AST.Expr -> Either Error Expression
analyzeExpr _ (AST.IntConst  i) = return (IntegerLiteral i)
analyzeExpr _ (AST.BoolConst b) = return (BooleanLiteral b)
analyzeExpr env (AST.LVal lval) = Ref <$> analyzeLValue env lval
analyzeExpr env (AST.Unary op e) = Unary op <$> analyzeExpr env `guard` e .: d
  where
    d = unaryOpDomain op
analyzeExpr env (AST.Binary op a b) = Binary op <$> a' <*> b'
  where
    a' = analyzeExpr env `guard` a .: d1
    b' = analyzeExpr env `guard` b .: d2
    (d1, d2) = binaryOpDomain op
analyzeExpr env (AST.Equals s a b) =
  do a' <- analyzeExpr env a
     b' <- analyzeExpr env `guard` b .: typeOf a'
     return (Equals s a' b')
analyzeExpr env (AST.Ternary c a b) =
  do c' <- analyzeExpr env `guard` c .: Boolean
     a' <- analyzeExpr env a
     b' <- analyzeExpr env `guard` b .: typeOf a'
     return (IfElse c' a' b')
analyzeExpr env (AST.FunApp f args) =
  do FunType ts r <- lookupFunction f env
     args' <- analyzeExpr env `guardAll` args .: ts
     return (FunApp (Typed f r) args')