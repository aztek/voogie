{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Voogie.Front (analyze) where

import Control.Monad (foldM)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader (MonadTrans, lift, ReaderT(..), withReaderT, ask, local)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Voogie.NonEmpty as VNE (zipWithM)
import qualified Data.Map as Map (empty, insert, lookup, toList)
import Data.Map (Map)
import Data.Maybe (catMaybes)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup(..))
#endif

import qualified Voogie.AST as A
import qualified Voogie.AST.Boogie as AST
import qualified Voogie.AST.FOOL as F.AST
import Voogie.Boogie (Boogie)
import qualified Voogie.Boogie.Smart as B
import Voogie.Error
import qualified Voogie.FOOL.Smart as F
import Voogie.Pretty.Boogie.Boogie
import Voogie.Theory

newtype Env a = Env (Map a Type)

instance Ord a => Semigroup (Env a) where
  Env m1 <> Env m2 = Env (m1 <> m2)

emptyEnv :: Env a
emptyEnv = Env Map.empty

type AnalyzeE a t = ReaderT (Env a) Result t

liftMT :: Monad m => (r -> m a) -> ReaderT r m a
liftMT f = do { e <- ask; lift (f e) }

localM :: Monad m => ReaderT r m r -> ReaderT r m a -> ReaderT r m a
localM m rma = do { r <- m; local (const r) rma }

lookupEnv :: (Ord a, Named a) => A.AST a -> AnalyzeE a (A.AST (Typed a))
lookupEnv = liftMT . \ast (Env vs) -> case Map.lookup (A.astValue ast) vs of
  Just  t -> Right (Typed t <$> ast)
  Nothing -> Left  (UndefinedVariable ast)

extendEnv :: (Ord a, Named a) => Typed (A.AST a) -> AnalyzeE a (Env a)
extendEnv = liftMT . extend
  where
    extend (Typed t ast) env@(Env vs) = case runReaderT (lookupEnv ast) env of
      Right tast' -> Left (MultipleDefinitions tast')
      Left _ -> Right (Env (Map.insert (A.astValue ast) t vs))

extendEnvT :: (Ord a, Named a, Traversable t)
           => t (Typed (A.AST a)) -> AnalyzeE a (Env a)
extendEnvT ts = do { env <- ask; foldM (local . const) env (fmap extendEnv ts) }

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

typed :: (TypeOf a, Pretty a) => Type -> A.AST a -> Result a
typed t (A.AST pos a)
  | t == typeOf a = Right a
  | otherwise = Left (TypeMismatch (A.AST pos (Typed (typeOf a) a)) t)

typedM :: (Pretty a, TypeOf a, Monad m)
       => Type -> A.AST (m a) -> m (Result a)
typedM t = fmap (typed t) . sequence

infix 3 <::>
(<::>) :: (Pretty a, TypeOf a, MonadTrans t, Monad (t Result))
       => A.AST (t Result a) -> Type -> t Result a
ast <::> t = t `typedM` ast >>= lift

guardType :: (Pretty a, TypeOf a, MonadTrans t, Monad (t Result))
          => (b -> t Result a) -> A.AST b -> Type -> t Result a
guardType f a t = f <$> a <::> t

analyzeTopLevel :: AST.TopLevel -> Analyze (Maybe B.TopLevel)
analyzeTopLevel = \case
  Left stmt -> fmap Left <$> analyzeStmt (A.astValue stmt)
  Right prop -> Just . Right <$> analyzeProp prop

analyzeStmts :: [AST.Stmt] -> Analyze [B.Statement]
analyzeStmts = fmap catMaybes . mapM (analyzeStmt . A.astValue)

analyzeStmt :: AST.Stmt' -> Analyze (Maybe B.Statement)
analyzeStmt = \case
  AST.If c a b -> B.if_ <$> (analyzeExpr <$> c <::> Boolean)
                        <*> analyzeStmts a <*> analyzeStmts b
  AST.Assign ass -> B.assign <$> mapM analyzeAssignment ass

analyzeProp :: AST.Prop -> Analyze B.Property
analyzeProp = \case
  AST.Assume f -> B.assume <$> analyzeProperty f
  AST.Assert f -> B.assert <$> analyzeProperty f

analyzeAssignment :: (AST.LVal, AST.Expr) -> Analyze B.Assignment
analyzeAssignment (lval, e) = do
  lv <- analyzeLValue lval
  e' <- analyzeExpr <$> e <::> typeOf lv
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
  es <- mapM (uncurry . VNE.zipWithM $ guardType analyzeExpr) is'
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
    e' <- analyzeExpr <$> e <::> d
    return (B.unary op e')
  AST.Binary op a b -> do
    let (d1, d2) = binaryOpDomain op
    a' <- analyzeExpr <$> a <::> d1
    b' <- analyzeExpr <$> b <::> d2
    return (B.binary op a' b')
  AST.Equals s a b -> do
    a' <- analyzeExpr (A.astValue a)
    b' <- analyzeExpr <$> b <::> typeOf a'
    return (B.equals s a' b')
  AST.Ternary c a b -> do
    c' <- analyzeExpr <$> c <::> Boolean
    a' <- analyzeExpr (A.astValue a)
    b' <- analyzeExpr <$> b <::> typeOf a'
    return (B.ifElse c' a' b')

analyzeProperty :: F.AST.Term -> Analyze F.Formula
analyzeProperty t = withReaderT (emptyEnv,) (analyzeFormula t)

type AnalyzeF t = ReaderT (Env F.Var, Env Name) Result t

analyzeFormula :: F.AST.Term -> AnalyzeF F.Formula
analyzeFormula f = analyzeTerm <$> f <::> Boolean

analyzeTerm :: F.AST.Term' -> AnalyzeF F.Term
analyzeTerm = \case
  F.AST.IntConst  i -> return (F.integerConstant i)
  F.AST.BoolConst b -> return (F.booleanConstant b)
  F.AST.Ref ast is -> do
    var <- analyzeName ast
    A.astValue <$> foldM analyzeSelect var is
  F.AST.Unary op t -> do
    let d = unaryOpDomain op
    t' <- analyzeTerm <$> t <::> d
    return (F.unary op t')
  F.AST.Binary op a b -> do
    let (d1, d2) = binaryOpDomain op
    a' <- analyzeTerm <$> a <::> d1
    b' <- analyzeTerm <$> b <::> d2
    return (F.binary op a' b')
  F.AST.Ternary c a b -> do
    c' <- analyzeFormula c
    a' <- analyzeTerm (A.astValue a)
    b' <- analyzeTerm <$> b <::> typeOf a'
    return (F.if_ c' a' b')
  F.AST.Equals s a b -> do
    a' <- analyzeTerm (A.astValue a)
    b' <- analyzeTerm <$> b <::> typeOf a'
    return (F.equals s a' b')
  F.AST.Quantified q vars f -> do
    let flatVars = fmap (fmap F.var <$>) (sequence =<< vars)
    qv <- lift $ runReaderT (extendEnvT flatVars) emptyEnv
    f' <- local (first (<> qv)) (analyzeFormula f)
    let vars' = fmap (fmap A.astValue) flatVars
    return (F.quantify q vars' f')

instance Semigroup (m a) => Semigroup (ReaderT r m a) where
  ReaderT f <> ReaderT g = ReaderT (f <> g)

analyzeName :: A.AST Name -> AnalyzeF (A.AST F.Term)
analyzeName ast = withReaderT fst analyzeVar
               <> withReaderT snd analyzeSym
  where
    analyzeVar = fmap F.variable <$> lookupEnv (F.var <$> ast)
    analyzeSym = fmap F.constant <$> lookupEnv ast

analyzeSelect :: A.AST F.Term -> NonEmpty F.AST.Term -> AnalyzeF (A.AST F.Term)
analyzeSelect ast@(A.AST pos term) as = case typeOf term of
  Array ts _ -> A.AST pos <$> (F.select term <$> VNE.zipWithM (guardType analyzeTerm) as ts)
  t -> lift $ Left (NonArraySelect (Typed t <$> ast))
