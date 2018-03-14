{-# LANGUAGE PatternGuards #-}

module Voogie.Front (analyze) where

import Control.Monad (foldM, join)
import Control.Monad.Extra (mapMaybeM)
import Data.Maybe
import Data.Foldable

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Voogie.NonEmpty as VNE

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Voogie.Theory
import Voogie.BoogiePretty

import qualified Voogie.AST as A

import Voogie.Boogie (Boogie)
import qualified Voogie.Boogie.Smart as B
import qualified Voogie.Boogie.AST as AST
import Voogie.Boogie.BoogiePretty()

import qualified Voogie.FOOL.Smart as F
import qualified Voogie.FOOL.AST as F.AST
import Voogie.FOOL.BoogiePretty()

type Error = String
type Result = Either Error

newtype Env = Env (Map Name Type)

emptyEnv :: Env
emptyEnv = Env Map.empty

lookupVariable :: Name -> Env -> Result (Typed Name)
lookupVariable n (Env vs)
  | Just t <- Map.lookup n vs = Right (Typed t n)
  | otherwise = Left ("undefined variable " ++ n)

insertVariable :: Typed Name -> Env -> Env
insertVariable (Typed t n) (Env vs) = Env (Map.insert n t vs)

extendEnv :: Typed Name -> Env -> Result Env
extendEnv v@(Typed _ n) env
  | Left _ <- lookupVariable n env = Right (insertVariable v env)
  | otherwise = Left ("redefined variable " ++ n)

variables :: Env -> [Typed Name]
variables (Env vs) = fmap (\(n, t) -> Typed t n) (Map.toList vs)

analyze :: AST.Boogie -> Result Boogie
analyze (AST.Boogie globals main) = do
  env <- foldrM analyzeDecl emptyEnv globals
  (env', main') <- analyzeMain env main
  return (B.boogie (variables env') main')

analyzeMain :: Env -> AST.Main -> Result (Env, B.Main)
analyzeMain env (AST.Main modifies pre returns locals toplevel post) = do
  env'  <- foldrM analyzeDecl env locals
  env'' <- foldrM extendEnv env' returnVars
  pre' <- mapM (analyzeProperty env) pre
  toplevel' <- analyzeTopLevels env'' toplevel
  post' <- mapM (analyzeProperty env'') post
  return (env'', B.main modifies pre' toplevel' post')
  where
    returnVars = case returns of
      Just (AST.Returns r) -> NE.toList r
      Nothing -> []

analyzeDecl :: AST.Decl -> Env -> Result Env
analyzeDecl (AST.Declare ns) env = foldrM extendEnv env (sequence ns)

guardType :: (TypeOf b, BoogiePretty b)
          => (a -> Result b) -> Type -> A.AST a -> Result b
guardType analyze t (A.AST pos a) = do
  b <- analyze a
  let t' = typeOf b
  if t == t'
  then return b
  else Left $ show pos ++ ": Type error: expected an expression of the type " ++
              pretty t ++ " but got " ++ pretty b ++ " of the type " ++ pretty t'

infix 6 <:$>
(<:$>) :: (TypeOf b, BoogiePretty b)
       => (a -> Result b) -> A.AST a -> Type -> Result b
f <:$> a = \t -> guardType f t a

infix 6 `guardAll`
guardAll :: (TypeOf b, BoogiePretty b)
         => (a -> Result b) -> NonEmpty (A.AST a) -> NonEmpty Type
         -> Result (NonEmpty b)
guardAll f as ts = VNE.zipWithM (guardType f) ts as

infix 5 .:
(.:) :: (a -> b) -> a -> b
(.:) = ($)

analyzeTopLevels :: Env -> [Either AST.Stmt AST.Assume] -> Result [B.TopLevel]
analyzeTopLevels env = mapMaybeM analyzeTopLevel
  where
    analyzeTopLevel :: Either AST.Stmt AST.Assume -> Result (Maybe B.TopLevel)
    analyzeTopLevel (Left stmt) = fmap Left <$> analyzeStmt (A.astValue stmt)
    analyzeTopLevel (Right assume) = Just . Right <$> analyzeAssume assume

    analyzeStmts :: [AST.Stmt] -> Result [B.Statement]
    analyzeStmts = fmap catMaybes . mapM (analyzeStmt . A.astValue)

    analyzeStmt :: AST.Stmt' -> Result (Maybe B.Statement)
    analyzeStmt (AST.If c a b) = B.if_ <$> analyzeExpr <:$> c .: Boolean
                                       <*> analyzeStmts a
                                       <*> analyzeStmts b
    analyzeStmt (AST.Assign ass) = B.assign <$> mapM analyzeAssignment ass

    analyzeAssume :: AST.Assume -> Result B.Assume
    analyzeAssume (AST.Assume f) = B.assume <$> analyzeProperty env f

    analyzeAssignment :: (AST.LVal, AST.Expr) -> Result B.Assignment
    analyzeAssignment (lval, e) = do
      lv <- analyzeLValue lval
      e' <- analyzeExpr <:$> e .: typeOf lv
      return (lv, e')

    analyzeLValue :: AST.LVal -> Result B.LValue
    analyzeLValue (AST.Ref n is) = do
      var <- lookupVariable n env
      let t = typeOf var
      let ais = arrayIndexes t
      xs <- if length is > length ais
            then Left ("Too many array indexes for type " ++ show t)
            else Right (zip is ais)
      B.lvalue var <$> mapM (uncurry $ guardAll analyzeExpr) xs

    analyzeExpr :: AST.Expr' -> Result B.Expression
    analyzeExpr (AST.IntConst  i) = return (B.integerLiteral i)
    analyzeExpr (AST.BoolConst b) = return (B.booleanLiteral b)
    analyzeExpr (AST.LVal lval) = do
      lval' <- analyzeLValue lval
      return (B.ref lval')
    analyzeExpr (AST.Unary op e) = do
      let d = unaryOpDomain op
      e' <- analyzeExpr <:$> e .: d
      return (B.unary op e')
    analyzeExpr (AST.Binary op a b) = do
      let (d1, d2) = binaryOpDomain op
      a' <- analyzeExpr <:$> a .: d1
      b' <- analyzeExpr <:$> b .: d2
      return (B.binary op a' b')
    analyzeExpr (AST.Equals s a b) = do
      a' <- analyzeExpr (A.astValue a)
      b' <- analyzeExpr <:$> b .: typeOf a'
      return (B.equals s a' b')
    analyzeExpr (AST.Ternary c a b) = do
      c' <- analyzeExpr <:$> c .: Boolean
      a' <- analyzeExpr (A.astValue a)
      b' <- analyzeExpr <:$> b .: typeOf a'
      return (B.ifElse c' a' b')

analyzeProperty :: Env -> F.AST.Term -> Result F.Formula
analyzeProperty env = analyzeFormula (env, Set.empty)

type Context = (Env, Set (Typed Name))

extendContext :: Typed Name -> Context -> Context
extendContext v (env, qv) = (insertVariable v env, Set.insert v qv)

analyzeFormula :: Context -> F.AST.Term -> Result F.Formula
analyzeFormula ctx@(env, qv) f = analyzeTerm <:$> f .: Boolean
  where
    analyzeTerm :: F.AST.Term' -> Result F.Term
    analyzeTerm (F.AST.IntConst  i) = return (F.integerConstant i)
    analyzeTerm (F.AST.BoolConst b) = return (F.booleanConstant b)
    analyzeTerm (F.AST.Ref n is) = do
      var <- analyzeVar n
      foldM analyzeSelect var is
    analyzeTerm (F.AST.Unary op t) = do
      let d = unaryOpDomain op
      t' <- analyzeTerm <:$> t .: d
      return (F.unary op t')
    analyzeTerm (F.AST.Binary op a b) = do
      let (d1, d2) = binaryOpDomain op
      a' <- analyzeTerm <:$> a .: d1
      b' <- analyzeTerm <:$> b .: d2
      return (F.binary op a' b')
    analyzeTerm (F.AST.Ternary c a b) = do
      c' <- analyzeFormula ctx c
      a' <- analyzeTerm (A.astValue a)
      b' <- analyzeTerm <:$> b .: typeOf a'
      return (F.if_ c' a' b')
    analyzeTerm (F.AST.Equals s a b) = do
      a' <- analyzeTerm (A.astValue a)
      b' <- analyzeTerm <:$> b .: typeOf a'
      return (F.equals s a' b')
    analyzeTerm (F.AST.Quantified q vars f) = do
      (ctx', vars') <- analyzeVarList vars
      f' <- analyzeFormula ctx' f
      return (F.quantify q vars' f')

    analyzeVar :: Name -> Result F.Term
    analyzeVar n = do
      v <- lookupVariable n env
      return $ if v `Set.member` qv
               then F.variable (F.var <$> v)
               else F.constant v

    analyzeSelect :: F.Term -> NonEmpty F.AST.Term -> Result F.Term
    analyzeSelect term as = case typeOf term of
      Array ts _ -> F.select term <$> analyzeTerm `guardAll` as .: ts
      t -> Left ("expected an expression of an array type," ++
                 " but got " ++ pretty term ++ " of the type " ++ pretty t)

    analyzeVarList :: F.AST.VarList -> Result (Context, F.VarList)
    -- TODO: check that the variables are disjoint
    analyzeVarList vars = Right (ctx', fmap (fmap F.var) vars')
      where
        vars' = join (fmap sequence vars)
        ctx' = foldr extendContext ctx vars'
