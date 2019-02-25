module Voogie.Front (analyze) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Maybe

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Voogie.NonEmpty as VNE

import qualified Data.Map as Map
import Data.Map (Map)

import Voogie.Error
import Voogie.Theory
import Voogie.BoogiePretty()

import qualified Voogie.AST as A

import Voogie.Boogie (Boogie)
import qualified Voogie.Boogie.Smart as B
import qualified Voogie.Boogie.AST as AST
import Voogie.Boogie.BoogiePretty()

import qualified Voogie.FOOL.Smart as F
import qualified Voogie.FOOL.AST as F.AST
import Voogie.FOOL.BoogiePretty()

import Text.PrettyPrint.ANSI.Leijen (Pretty)

data Env a = Env (Map a Type)

instance Ord a => Semigroup (Env a) where
  Env m1 <> Env m2 = Env (m1 <> m2)

emptyEnv :: (Ord a, Named a) => Env a
emptyEnv = Env Map.empty

type AnalyzeE a t = ReaderT (Env a) Result t

lookupEnv :: (Ord a, Named a) => A.AST a -> AnalyzeE a (A.AST (Typed a))
lookupEnv ast = do
  Env vs <- ask
  lift $ case Map.lookup (A.astValue ast) vs of
    Just t -> Right (Typed t <$> ast)
    Nothing -> Left (UndefinedVariable ast)

extendEnv :: (Ord a, Named a) => Typed (A.AST a) -> AnalyzeE a (Env a)
extendEnv tast = do
  env <- ask
  lift $ case runReaderT (lookupEnv $ valueOf tast) env of
    Right tast' -> Left (MultipleDefinitions tast')
    Left _ -> Right (insertEnv (A.astValue <$> tast) env)
  where
    insertEnv :: (Ord a, Named a) => Typed a -> Env a -> Env a
    insertEnv (Typed t n) (Env vs) = Env (Map.insert n t vs)

extendEnvT :: (Ord a, Named a, Traversable t)
           => t (Typed (A.AST a)) -> AnalyzeE a (Env a)
extendEnvT ts = do
  env <- ask
  foldM (local . const) env (fmap extendEnv ts)

localM :: Monad m => ReaderT r m r -> ReaderT r m a -> ReaderT r m a
localM m rma = do { r <- m; local (const r) rma }

type Analyze t = AnalyzeE Name t

analyze :: AST.Boogie -> Result Boogie
analyze boogie = runReaderT (analyzeBoogie boogie) emptyEnv

analyzeBoogie :: AST.Boogie -> Analyze Boogie
analyzeBoogie (AST.Boogie globals main) = do
  (Env vs, main') <- localM (analyzeDecls globals) (analyzeMain main)
  let vs' = fmap (\(n, t) -> Typed t n) (Map.toList vs)
  return (B.boogie vs' main')

analyzeMain :: AST.Main -> Analyze (Env Name, B.Main)
analyzeMain (AST.Main modifies pre returns locals toplevel post) = do
  pre' <- mapM analyzeProperty pre

  env <- localM (analyzeDecls locals) (extendEnvT returns')

  toplevel' <- local (const env) (mapMaybeM analyzeTopLevel toplevel)
  post' <- local (const env) (mapM analyzeProperty post)

  return (env, B.main modifies' pre' toplevel' post')
  where
    modifies' = fmap A.astValue modifies
    returns' = maybe [] (\(AST.Returns r) -> NE.toList r) returns

analyzeDecls :: [AST.Decl] -> Analyze (Env Name)
analyzeDecls = extendEnvT
             . concatMap (\(AST.Declare ns) -> NE.toList (sequence ns))

infix 6 <:$>
(<:$>) :: (TypeOf b, Pretty b)
          => (a -> Analyze b) -> A.AST a -> Type -> Analyze b
(<:$>) analyze (A.AST pos a) t = do
  b <- analyze a
  let t' = typeOf b
  lift $ if t == t'
         then Right b
         else Left (TypeMismatch (A.AST pos (Typed t' b)) t)

infix 6 `guardAll`
guardAll :: (TypeOf b, Pretty b)
         => (a -> Analyze b) -> NonEmpty (A.AST a) -> NonEmpty Type
         -> Analyze (NonEmpty b)
guardAll = VNE.zipWithM . (<:$>)

infix 5 .:
(.:) :: (a -> b) -> a -> b
(.:) = ($)

analyzeTopLevel :: AST.TopLevel -> Analyze (Maybe B.TopLevel)
analyzeTopLevel = \case
  Left stmt -> fmap Left <$> analyzeStmt (A.astValue stmt)
  Right assume -> Just . Right <$> analyzeAssume assume

analyzeStmts :: [AST.Stmt] -> Analyze [B.Statement]
analyzeStmts = fmap catMaybes . mapM (analyzeStmt . A.astValue)

analyzeStmt :: AST.Stmt' -> Analyze (Maybe B.Statement)
analyzeStmt = \case
  AST.If c a b -> B.if_ <$> analyzeExpr <:$> c .: Boolean
                        <*> analyzeStmts a <*> analyzeStmts b
  AST.Assign ass -> B.assign <$> mapM analyzeAssignment ass

analyzeAssume :: AST.Assume -> Analyze B.Assume
analyzeAssume (AST.Assume f) = B.assume <$> analyzeProperty f

analyzeAssignment :: (AST.LVal, AST.Expr) -> Analyze B.Assignment
analyzeAssignment (lval, e) = do
  lv <- analyzeLValue lval
  e' <- analyzeExpr <:$> e .: typeOf lv
  return (lv, e')

analyzeLValue :: AST.LVal -> Analyze B.LValue
analyzeLValue (AST.Ref ast is) = do
  var <- lookupEnv ast
  let n = A.astValue var
  let t = typeOf n
  let ais = arrayIndexes t
  is' <- lift $ if length is > length ais
                then Left (ArrayDimensionMismatch (Typed t <$> var))
                else Right (zip is ais)
  es <- mapM (uncurry $ guardAll analyzeExpr) is'
  return (B.lvalue n es)

analyzeExpr :: AST.Expr' -> Analyze B.Expression
analyzeExpr = \case
  AST.IntConst  i -> return (B.integerLiteral i)
  AST.BoolConst b -> return (B.booleanLiteral b)
  AST.LVal lval -> do
    lval' <- analyzeLValue lval
    return (B.ref lval')
  AST.Unary op e -> do
    let d = unaryOpDomain op
    e' <- analyzeExpr <:$> e .: d
    return (B.unary op e')
  AST.Binary op a b -> do
    let (d1, d2) = binaryOpDomain op
    a' <- analyzeExpr <:$> a .: d1
    b' <- analyzeExpr <:$> b .: d2
    return (B.binary op a' b')
  AST.Equals s a b -> do
    a' <- analyzeExpr (A.astValue a)
    b' <- analyzeExpr <:$> b .: typeOf a'
    return (B.equals s a' b')
  AST.Ternary c a b -> do
    c' <- analyzeExpr <:$> c .: Boolean
    a' <- analyzeExpr (A.astValue a)
    b' <- analyzeExpr <:$> b .: typeOf a'
    return (B.ifElse c' a' b')

analyzeProperty :: F.AST.Term -> Analyze F.Formula
analyzeProperty = analyzeFormula emptyEnv

type QV = Env F.Var

analyzeFormula :: QV -> F.AST.Term -> Analyze F.Formula
analyzeFormula qv f = analyzeTerm qv <:$> f .: Boolean

analyzeTerm :: QV -> F.AST.Term' -> Analyze F.Term
analyzeTerm qv = \case
  F.AST.IntConst  i -> return (F.integerConstant i)
  F.AST.BoolConst b -> return (F.booleanConstant b)
  F.AST.Ref ast is -> do
    var <- analyzeName qv ast
    A.astValue <$> foldM (analyzeSelect qv) var is
  F.AST.Unary op t -> do
    let d = unaryOpDomain op
    t' <- analyzeTerm qv <:$> t .: d
    return (F.unary op t')
  F.AST.Binary op a b -> do
    let (d1, d2) = binaryOpDomain op
    a' <- analyzeTerm qv <:$> a .: d1
    b' <- analyzeTerm qv <:$> b .: d2
    return (F.binary op a' b')
  F.AST.Ternary c a b -> do
    c' <- analyzeFormula qv c
    a' <- analyzeTerm qv (A.astValue a)
    b' <- analyzeTerm qv <:$> b .: typeOf a'
    return (F.if_ c' a' b')
  F.AST.Equals s a b -> do
    a' <- analyzeTerm qv (A.astValue a)
    b' <- analyzeTerm qv <:$> b .: typeOf a'
    return (F.equals s a' b')
  F.AST.Quantified q vars f -> do
    let flatVars = fmap (fmap F.var <$>) (sequence =<< vars)
    qv' <- lift $ runReaderT (extendEnvT flatVars) emptyEnv
    f' <- analyzeFormula (qv <> qv') f
    let vars' = fmap (fmap A.astValue) flatVars
    return (F.quantify q vars' f')

analyzeName :: QV -> A.AST Name -> Analyze (A.AST F.Term)
analyzeName qv ast = do
  env <- ask
  let analyzeVar = fmap F.variable <$> runReaderT (lookupEnv (F.var <$> ast)) qv
  let analyzeSym = fmap F.constant <$> runReaderT (lookupEnv ast) env
  lift $ analyzeVar <> analyzeSym

analyzeSelect :: QV -> A.AST F.Term -> NonEmpty F.AST.Term -> Analyze (A.AST F.Term)
analyzeSelect qv ast@(A.AST pos term) as = case typeOf term of
  Array ts _ -> A.AST pos <$> (F.select term <$> analyzeTerm qv `guardAll` as .: ts)
  t -> lift $ Left (NonArraySelect (Typed t <$> ast))
