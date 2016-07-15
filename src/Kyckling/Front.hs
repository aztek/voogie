module Kyckling.Front where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Bifunctor

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


flattenDeclarations :: [AST.Stmt] -> [AST.Stmt]
flattenDeclarations = foldr ((++) . flattenDeclaration) []

flattenDeclaration :: AST.Stmt -> [AST.Stmt]
flattenDeclaration (AST.Declare t defs) = concatMap toStmts defs
  where
    toStmts (n, e) = AST.Declare t [(n, Nothing)] : maybeToList (fmap (AST.Update (AST.Var n) AST.Assign) e)
flattenDeclaration (AST.If c a b) = [AST.If c (flattenDeclarations a) (flattenDeclarations b)]
flattenDeclaration s = [s]


analyze :: AST.AST -> Either Error Program
analyze (AST.AST fs ss as) =
  do fs' <- analyzeFunDefs fs
     (env, ss') <- analyzeNonTerminating emptyEnv (flattenDeclarations ss)
     as' <- mapM (analyzeAssert env) as
     return (Program fs' ss' as')

analyzeFunDefs :: [AST.FunDef] -> Either Error [FunDef]
analyzeFunDefs = mapM analyzeFunDef

analyzeFunDef :: AST.FunDef -> Either Error FunDef
analyzeFunDef (AST.FunDef t n vars stmts) =
  do let env = foldr (\(Typed n t) -> Map.insert n t) emptyEnv vars
     (_, ts) <- analyzeTerminating env stmts
     return (FunDef t n vars ts)

analyzeTerminating :: Env -> [AST.Stmt] -> Either Error (Env, TerminatingStatement)
analyzeTerminating env ss =
  do (env', ss') <- analyzeStmts env ss
     case ss' of
       Left  ts  -> return (env', ts)
       Right ss' -> Left "non-terminating statement in a terminating block"

analyzeNonTerminating :: Env -> [AST.Stmt] -> Either Error (Env, [Statement])
analyzeNonTerminating env ss =
  do (env', ss') <- analyzeStmts env ss
     case ss' of
       Left  ts  -> Left "terminating statement in a non-terminating block"
       Right ss' -> return (env', ss')


analyzeStmts :: Env -> [AST.Stmt] -> Either Error (Env, Either TerminatingStatement [Statement])
analyzeStmts env [] = Right (env, Right [])
analyzeStmts env (s:ss) =
  do (env',  s')  <- analyzeStmt env s
     case s' of
       Left ts  -> return (env', Left ts)
       Right s' -> do (env'', ss') <- analyzeStmts env' ss
                      return (env'', bimap (appendStatement s') (s':) ss')
  where
    appendStatement :: Statement -> TerminatingStatement -> TerminatingStatement
    appendStatement s (Return    ss e)     = Return    (s:ss) e
    appendStatement s (IteReturn ss c a b) = IteReturn (s:ss) c a b


analyzeStmt :: Env -> AST.Stmt -> Either Error (Env, Either TerminatingStatement Statement)
analyzeStmt env (AST.Declare t [(n, Nothing)]) =
  if Map.member n env
  then Left $ "the variable " ++ n ++ " shadows the previous definition"
  else let env' = Map.insert n t env
           stmt = Declare (Typed n t)
        in return (env', Right stmt)
analyzeStmt env (AST.Declare t _) = error "should be eliminated by flattenDeclaration"
analyzeStmt env (AST.If c a b) =
  do (_, a') <- analyzeStmts env a
     (_, b') <- analyzeStmts env b
     c' <- guardType Boolean (analyzeExpr env) c
     let stmt = case (a', b') of
                  (Right a', Right b') -> Right $ If c' a' b'
                  (Right a', Left  b') -> Right $ IfTerminating c' False a' b'
                  (Left  a', Right b') -> Right $ IfTerminating c' True  b' a'
                  (Left  a', Left  b') -> Left  $ IteReturn [] c' a' b'
     return (env, stmt)
analyzeStmt env (AST.Increment lval) =
  do lv <- guardType Integer (analyzeLValue env) lval
     let stmt = Assign lv (Binary Add (Ref lv) (IntegerLiteral 1))
     return (env, Right stmt)
analyzeStmt env (AST.Decrement lval) =
  do lv <- guardType Integer (analyzeLValue env) lval
     let stmt = Assign lv (Binary Subtract (Ref lv) (IntegerLiteral 1))
     return (env, Right stmt)
analyzeStmt env (AST.Update lval AST.Assign e) =
  do lv <- analyzeLValue env lval
     e' <- guardType (typeOf lv) (analyzeExpr env) e
     let stmt = Assign lv e'
     return (env, Right stmt)
analyzeStmt env (AST.Update lval op e) =
  do lv <- guardType d1 (analyzeLValue env) lval
     e' <- guardType d2 (analyzeExpr env) e
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
     let stmt = Return [] e'
     return (env, Left stmt)

analyzeAssert :: Env -> AST.Assert -> Either Error Assertion
analyzeAssert env (AST.Assert f) = Assertion <$> analyzeFormula env f

analyzeFormula :: Env -> F.AST.Term -> Either Error F.Formula
analyzeFormula env = guardType Boolean (analyzeTerm env)

analyzeTerm :: Env -> F.AST.Term -> Either Error F.Term
analyzeTerm _ (F.AST.IntConst  i) = return (F.IntegerConstant i)
analyzeTerm _ (F.AST.BoolConst b) = return (F.BooleanConstant b)
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
analyzeTerm env (F.AST.Equals s a b) =
  do a' <- analyzeTerm env a
     b' <- guardType (typeOf a') (analyzeTerm env) b
     return (F.Equals s a' b')
analyzeTerm env (F.AST.Quantified q vars term) = F.Quantify q vars' <$> analyzeFormula env' term
  where
    -- TODO: check that the variables are disjoint
    env' = foldr (\(Typed v t) -> Map.insert v t) env vars
    vars' = map (fmap F.Var) vars
analyzeTerm env (F.AST.Constant  s)   = F.Constant <$> lookupName s env
analyzeTerm env (F.AST.ArrayElem s i) = F.Select <$> (F.Constant <$> lookupArrayName s env) <*> guardType Integer (analyzeTerm env) i


analyzeLValue :: Env -> AST.LVal -> Either Error LValue
analyzeLValue env (AST.Var s) = Variable <$> lookupName s env
analyzeLValue env (AST.ArrayElem s i) = ArrayElem <$> lookupArrayName s env <*> guardType Integer (analyzeExpr env) i


analyzeExpr :: Env -> AST.Expr -> Either Error Expression
analyzeExpr _ (AST.IntConst  i) = return (IntegerLiteral i)
analyzeExpr _ (AST.BoolConst b) = return (BooleanLiteral b)
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
analyzeExpr env (AST.Equals s a b) =
  do a' <- analyzeExpr env a
     b' <- guardType (typeOf a') (analyzeExpr env) b
     return (Equals s a' b')
analyzeExpr env (AST.Ternary c a b) =
  do c' <- guardType Boolean (analyzeExpr env) c
     a' <- analyzeExpr env a
     b' <- guardType (typeOf a') (analyzeExpr env) b
     return (IfElse c' a' b')