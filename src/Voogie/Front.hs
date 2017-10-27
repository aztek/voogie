{-# LANGUAGE PatternGuards #-}

module Voogie.Front (
  analyze
) where

import Control.Monad (foldM, join)
import Control.Monad.Extra (mapMaybeM)
import Data.Maybe
import Data.Foldable

import Data.List.NonEmpty (NonEmpty)
import qualified Voogie.NonEmpty as VNE

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Voogie.Theory
import Voogie.Pretty

import Voogie.Boogie (Boogie)
import qualified Voogie.Boogie.Smart as B
import qualified Voogie.Boogie.AST as AST
import Voogie.Boogie.Pretty()

import qualified Voogie.FOOL.Smart as F
import qualified Voogie.FOOL.AST as F.AST
import Voogie.FOOL.Pretty()

type Error = String
type Result = Either Error

data Env = Env (Map Name Type)

emptyEnv :: Env
emptyEnv = Env Map.empty

lookupVariable :: Name -> Env -> Result (Typed Name)
lookupVariable n (Env vs)
  | Just t <- Map.lookup n vs = Right (Typed t n)
  | otherwise = Left ("undefined variable " ++ n)

insertVariable :: Typed Name -> Env -> Env
insertVariable (Typed t n) (Env vs) = Env (Map.insert n t vs)

variables :: Env -> [Typed Name]
variables (Env vs) = fmap (\(n, t) -> Typed t n) (Map.toList vs)

analyze :: AST.AST -> Result Boogie
analyze (AST.AST globalDecls (AST.Main pre r mainDecls ss post)) =
  do globalEnv <- foldrM analyzeDecl emptyEnv  globalDecls
     mainEnv'  <- foldrM analyzeDecl globalEnv mainDecls
     let mainEnv = case r of -- TODO: check for name clash
                     Just (AST.Returns r) -> foldr insertVariable mainEnv' r
                     Nothing -> mainEnv'
     ss'   <- analyzeMain mainEnv ss
     pre'  <- mapM (analyzeFormula globalEnv) pre
     post' <- mapM (analyzeFormula mainEnv)   post
     let main = B.main pre' ss' post'
     let vars = variables mainEnv
     return (B.boogie vars main)

analyzeDecl :: AST.Decl -> Env -> Result Env
analyzeDecl (AST.Declare (Typed t ns)) env = foldrM tryInsertVariable env ns
  where
    tryInsertVariable :: Name -> Env -> Result Env
    tryInsertVariable n env
      | Left _ <- lookupVariable n env = Right (insertVariable (Typed t n) env)
      | otherwise = Left ("redefined variable " ++ n)

guardType :: (TypeOf b, Pretty b) => (a -> Result b) -> Type -> a -> Result b
guardType analyze t a = do
  b <- analyze a
  let t' = typeOf b
  if t == t'
  then return b
  else Left $ "expected an expression of the type " ++ pretty t ++
              " but got " ++ pretty b ++ " of the type " ++ pretty t'

infix 6 <:$>
(<:$>) :: (TypeOf b, Pretty b) => (a -> Result b) -> a -> Type -> Result b
f <:$> a = \t -> guardType f t a

infix 6 `guardAll`
guardAll :: (TypeOf b, Pretty b)
         => (a -> Result b) -> NonEmpty a -> NonEmpty Type
         -> Result (NonEmpty b)
guardAll f as ts = VNE.zipWithM (guardType f) ts as

infix 5 .:
(.:) :: (a -> b) -> a -> b
(.:) = ($)

analyzeMain :: Env -> [Either AST.Stmt AST.Assume] -> Result [B.TopLevel]
analyzeMain env = mapMaybeM $ either (fmap (fmap   Left)  . analyzeStmt)
                                     (fmap (Just . Right) . analyzeAssume)
  where
    analyzeStmts :: [AST.Stmt] -> Result [B.Statement]
    analyzeStmts = fmap catMaybes . mapM analyzeStmt

    analyzeStmt :: AST.Stmt -> Result (Maybe B.Statement)
    analyzeStmt (AST.If c a b) = B.if_ <$> analyzeExpr c
                                       <*> analyzeStmts a
                                       <*> analyzeStmts b
    analyzeStmt (AST.Assign ass) = B.assign <$> mapM analyzeAssignment ass

    analyzeAssume :: AST.Assume -> Result B.Assume
    analyzeAssume (AST.Assume f) = B.assume <$> analyzeFormula env f

    analyzeAssignment :: (AST.LVal, AST.Expr) -> Result (B.LValue, B.Expression)
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

    analyzeExpr :: AST.Expr -> Result B.Expression
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
      a' <- analyzeExpr a
      b' <- analyzeExpr <:$> b .: typeOf a'
      return (B.equals s a' b')
    analyzeExpr (AST.Ternary c a b) = do
      c' <- analyzeExpr <:$> c .: Boolean
      a' <- analyzeExpr a
      b' <- analyzeExpr <:$> b .: typeOf a'
      return (B.ifElse c' a' b')

analyzeFormula :: Env -> F.AST.Term -> Result F.Formula
analyzeFormula env = analyzeFormula' (env, Set.empty)

type Context = (Env, Set (Typed Name))

extendContext :: Typed Name -> Context -> Context
extendContext v (env, qv) = (insertVariable v env, Set.insert v qv)

analyzeVar :: Context -> Name -> Result F.Term
analyzeVar (env, qv) n = do
  v <- lookupVariable n env
  return $ if v `Set.member` qv
           then F.variable (F.var <$> v)
           else F.constant v

analyzeFormula' :: Context -> F.AST.Term -> Result F.Formula
analyzeFormula' ctx f = analyzeTerm ctx <:$> f .: Boolean

analyzeTerm :: Context -> F.AST.Term -> Result F.Term
analyzeTerm _ (F.AST.IntConst  i) = return (F.integerConstant i)
analyzeTerm _ (F.AST.BoolConst b) = return (F.booleanConstant b)
analyzeTerm ctx (F.AST.Ref n is) = do
  var <- analyzeVar ctx n
  foldM (analyzeSelect ctx) var is
analyzeTerm ctx (F.AST.Unary op t) = do
  let d = unaryOpDomain op
  t' <- analyzeTerm ctx <:$> t .: d
  return (F.unary op t')
analyzeTerm ctx (F.AST.Binary op a b) = do
  let (d1, d2) = binaryOpDomain op
  a' <- analyzeTerm ctx <:$> a .: d1
  b' <- analyzeTerm ctx <:$> b .: d2
  return (F.binary op a' b')
analyzeTerm ctx (F.AST.Ternary c a b) = do
  c' <- analyzeFormula' ctx c
  a' <- analyzeTerm ctx a
  b' <- analyzeTerm ctx <:$> b .: typeOf a'
  return (F.if_ c' a' b')
analyzeTerm ctx (F.AST.Equals s a b) = do
  a' <- analyzeTerm ctx a
  b' <- analyzeTerm ctx <:$> b .: typeOf a'
  return (F.equals s a' b')
analyzeTerm ctx (F.AST.Quantified q vars f) = do
  (ctx', vars') <- analyzeVarList ctx vars
  f' <- analyzeFormula' ctx' f
  return (F.quantify q vars' f')

analyzeSelect :: Context -> F.Term -> NonEmpty F.AST.Term -> Result F.Term
analyzeSelect ctx term as = case typeOf term of
  Array ts _ -> F.select term <$> analyzeTerm ctx `guardAll` as .: ts
  t -> Left ("expected an expression of an array type," ++
             " but got " ++ pretty term ++ " of the type " ++ pretty t)

analyzeVarList :: Context -> F.AST.VarList -> Result (Context, F.VarList)
-- TODO: check that the variables are disjoint
analyzeVarList ctx vars = Right (ctx', fmap (fmap F.var) vars')
  where
    vars' = join (fmap sequence vars)
    ctx' = foldr extendContext ctx vars'