{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

{-|
Module       : Voogie.Front
Description  : Type checker of Boogie programs.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.Front (
  analyze
) where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader (MonadTrans, lift, ReaderT(..), withReaderT, ask, local)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE (zipWithM)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, lookup, toList)
import Data.Maybe (catMaybes)
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup(..))
#endif

import Voogie.AST (AST(..))
import qualified Voogie.AST.Boogie as B.AST
import qualified Voogie.AST.FOOL as F.AST
import qualified Voogie.Boogie.Smart as B
import Voogie.Error
import qualified Voogie.FOOL.Smart as F
import Voogie.Pretty.Boogie.Boogie (Pretty(..))
import Voogie.Language

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

lookupEnv :: (Ord a, Named a) => AST a -> AnalyzeE a (AST (Typed a))
lookupEnv = liftMT . \ast (Env vs) -> case Map.lookup (astValue ast) vs of
  Just  t -> Right (Typed t <$> ast)
  Nothing -> Left  (UndefinedVariable $ fmap nameOf ast)

extendEnv :: (Ord a, Named a) => Typed (AST a) -> AnalyzeE a (Env a)
extendEnv = liftMT . extend
  where
    extend (Typed t ast) env@(Env vs) = case runReaderT (lookupEnv ast) env of
      Right tast' -> Left (MultipleDefinitions $ fmap (fmap nameOf) tast')
      Left _ -> Right (Env (Map.insert (astValue ast) t vs))

extendEnvT :: (Ord a, Named a, Traversable t)
           => t (Typed (AST a)) -> AnalyzeE a (Env a)
extendEnvT ts = do { env <- ask; foldM (local . const) env (fmap extendEnv ts) }

type Analyze t = AnalyzeE Name t

analyze :: B.AST.Boogie -> Result B.Boogie
analyze boogie = runReaderT (analyzeBoogie boogie) emptyEnv

analyzeBoogie :: B.AST.Boogie -> Analyze B.Boogie
analyzeBoogie (B.AST.Boogie globals main) = do
  (Env vs, main') <- localM (analyzeDeclarations globals) (analyzeMain main)
  let vs' = fmap (\(n, t) -> Typed t n) (Map.toList vs)
  return (B.boogie vs' main')

analyzeMain :: B.AST.Main -> Analyze (Env Name, B.Main)
analyzeMain (B.AST.Main modifies pre returns locals toplevel post) = do
  pre' <- mapM analyzeProperty pre

  env <- localM (analyzeDeclarations locals) (extendEnvT returns')

  toplevel' <- local (const env) (mapMaybeM analyzeTopLevel toplevel)
  post' <- local (const env) (mapM analyzeProperty post)

  return (env, B.main modifies' pre' toplevel' post')
  where
    modifies' = fmap astValue modifies
    returns' = maybe [] (toList . B.AST.getReturns) returns

analyzeDeclarations :: [B.AST.Declaration] -> Analyze (Env Name)
analyzeDeclarations = extendEnvT
                    . concatMap (toList . sequence . B.AST.getDeclaration)

typed :: (TypeOf a, Pretty a) => Type -> AST a -> Result a
typed t (AST pos a)
  | t == typeOf a = Right a
  | otherwise = Left (TypeMismatch (AST pos (Typed (typeOf a) (pretty a))) t)

typedM :: (Pretty a, TypeOf a, Monad m)
       => Type -> AST (m a) -> m (Result a)
typedM t = fmap (typed t) . sequence

infix 3 <::>
(<::>) :: (Pretty a, TypeOf a, MonadTrans t, Monad (t Result))
       => AST (t Result a) -> Type -> t Result a
ast <::> t = t `typedM` ast >>= lift

guardType :: (Pretty a, TypeOf a, MonadTrans t, Monad (t Result))
          => (b -> t Result a) -> AST b -> Type -> t Result a
guardType f a t = f <$> a <::> t

guardTypes :: (Pretty a, TypeOf a, MonadTrans t, Monad (t Result))
           => (b -> t Result a) -> NonEmpty (AST b) -> NonEmpty Type
           -> t Result (NonEmpty a)
guardTypes f = NE.zipWithM (guardType f)

guardArrayDimension :: (Pretty e, TypeOf e, Foldable f, MonadTrans t, Monad (t Result))
                    => AST e -> f a -> f b -> t Result (f a, f b)
guardArrayDimension (AST p e) is ts
  | length is == length ts = return (is, ts)
  | otherwise = lift . Left . ArrayDimensionMismatch
              $ AST p (Typed (typeOf e) (pretty e))

guardArraySelect :: (Pretty a, TypeOf a, MonadTrans t, Monad (t Result))
                 => AST a -> t Result (NonEmpty Type, Type)
guardArraySelect (AST p e) = case typeOf e of
  Array ts r -> return (ts, r)
  t -> lift . Left . NonArraySelect $ AST p (Typed t (pretty e))

analyzeTopLevel :: B.AST.TopLevel -> Analyze (Maybe B.TopLevel)
analyzeTopLevel = \case
  Left stmt -> fmap Left <$> analyzeStmt (astValue stmt)
  Right prop -> Just . Right <$> analyzeProp prop

analyzeStmts :: [B.AST.Statement] -> Analyze [B.Statement]
analyzeStmts = fmap catMaybes . mapM (analyzeStmt . astValue)

analyzeStmt :: B.AST.StatementF -> Analyze (Maybe B.Statement)
analyzeStmt = \case
  B.AST.If c a b -> B.if_ <$> (analyzeExpr <$> c <::> Boolean)
                          <*> analyzeStmts a <*> analyzeStmts b
  B.AST.Assign ass -> B.assign <$> mapM analyzeAssignment ass

analyzeProp :: B.AST.Property -> Analyze B.Property
analyzeProp = \case
  B.AST.Assume f -> B.assume <$> analyzeProperty f
  B.AST.Assert f -> B.assert <$> analyzeProperty f

analyzeAssignment :: B.AST.Assignment -> Analyze B.Assignment
analyzeAssignment (lval, e) = do
  lv <- analyzeLValue lval
  e' <- analyzeExpr <$> e <::> typeOf lv
  return (lv, e')

analyzeLValue :: B.AST.LValue -> Analyze B.LValue
analyzeLValue (B.AST.LValue ast is) = do
  var@(AST _ n) <- lookupEnv ast
  let ts = arrayIndexes (typeOf n)
  (is', ts') <- guardArrayDimension var is ts
  es <- zipWithM (guardTypes analyzeExpr) is' ts'
  return (B.lvalue n es)

analyzeExpr :: B.AST.ExpressionF -> Analyze B.Expression
analyzeExpr = \case
  B.AST.IntegerLiteral i -> return (B.integerLiteral i)
  B.AST.BooleanLiteral b -> return (B.booleanLiteral b)
  B.AST.Ref lval -> do
    lval' <- analyzeLValue lval
    return (B.ref lval')
  B.AST.Unary op e -> do
    let d = unaryOpDomain op
    e' <- analyzeExpr <$> e <::> d
    return (B.unary op e')
  B.AST.Binary op a b -> do
    let (d1, d2) = binaryOpDomain op
    a' <- analyzeExpr <$> a <::> d1
    b' <- analyzeExpr <$> b <::> d2
    return (B.binary op a' b')
  B.AST.Equals s a b -> do
    a' <- analyzeExpr (astValue a)
    b' <- analyzeExpr <$> b <::> typeOf a'
    return (B.equals s a' b')
  B.AST.IfElse c a b -> do
    c' <- analyzeExpr <$> c <::> Boolean
    a' <- analyzeExpr (astValue a)
    b' <- analyzeExpr <$> b <::> typeOf a'
    return (B.ifElse c' a' b')

analyzeProperty :: F.AST.Term -> Analyze F.Formula
analyzeProperty t = withReaderT (emptyEnv,) (analyzeFormula t)

type AnalyzeF t = ReaderT (Env F.Var, Env Name) Result t

analyzeFormula :: F.AST.Term -> AnalyzeF F.Formula
analyzeFormula f = analyzeTerm <$> f <::> Boolean

analyzeTerm :: F.AST.TermF -> AnalyzeF F.Term
analyzeTerm = \case
  F.AST.IntegerConstant i -> return (F.integerConstant i)
  F.AST.BooleanConstant b -> return (F.booleanConstant b)
  F.AST.Ref ast is -> do
    var <- analyzeName ast
    astValue <$> foldM analyzeSelect var is
  F.AST.Unary op t -> do
    let d = unaryOpDomain op
    t' <- analyzeTerm <$> t <::> d
    return (F.unary op t')
  F.AST.Binary op a b -> do
    let (d1, d2) = binaryOpDomain op
    a' <- analyzeTerm <$> a <::> d1
    b' <- analyzeTerm <$> b <::> d2
    return (F.binary op a' b')
  F.AST.IfElse c a b -> do
    c' <- analyzeFormula c
    a' <- analyzeTerm (astValue a)
    b' <- analyzeTerm <$> b <::> typeOf a'
    return (F.ifElse c' a' b')
  F.AST.Equals s a b -> do
    a' <- analyzeTerm (astValue a)
    b' <- analyzeTerm <$> b <::> typeOf a'
    return (F.equals s a' b')
  F.AST.Quantify q vars f -> do
    let flatVars = fmap (fmap F.var <$>) (sequence =<< vars)
    qv <- lift $ runReaderT (extendEnvT flatVars) emptyEnv
    f' <- local (first (<> qv)) (analyzeFormula f)
    let vars' = fmap (fmap astValue) flatVars
    return (F.quantify q vars' f')

instance Semigroup (m a) => Semigroup (ReaderT r m a) where
  ReaderT f <> ReaderT g = ReaderT (f <> g)

analyzeName :: AST Name -> AnalyzeF (AST F.Term)
analyzeName ast = withReaderT fst analyzeVar
               <> withReaderT snd analyzeSym
  where
    analyzeVar = fmap F.variable <$> lookupEnv (F.var <$> ast)
    analyzeSym = fmap F.constant <$> lookupEnv ast

analyzeSelect :: AST F.Term -> NonEmpty F.AST.Term -> AnalyzeF (AST F.Term)
analyzeSelect ast@(AST pos term) as = do
  (ts, _) <- guardArraySelect ast
  (as', ts') <- guardArrayDimension ast as ts
  is <- guardTypes analyzeTerm as' ts'
  return $ AST pos (F.select term is)
