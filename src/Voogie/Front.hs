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

lookupEnv :: (Ord a, Named a) => A.AST a -> Env a -> Result (A.AST (Typed a))
lookupEnv ast (Env vs)
  | Just t <- Map.lookup (A.astValue ast) vs = Right (Typed t <$> ast)
  | otherwise = Left (UndefinedVariable ast)

insertEnv :: (Ord a, Named a) => Typed a -> Env a -> Env a
insertEnv (Typed t n) (Env vs) = Env (Map.insert n t vs)

extendEnv :: (Ord a, Named a) => Env a -> Typed (A.AST a) -> Result (Env a)
extendEnv env (Typed t ast@(A.AST pos n))
  | Left _ <- lookupEnv ast env = Right (insertEnv var env)
  | otherwise = Left (MultipleDefinitions (A.AST pos var))
  where var = Typed t n

type Analyze t = ReaderT (Env Name) Result t

analyze :: AST.Boogie -> Result Boogie
analyze boogie = runReaderT (analyzeBoogie boogie) emptyEnv

analyzeBoogie :: AST.Boogie -> Analyze Boogie
analyzeBoogie (AST.Boogie globals main) = do
  env <- analyzeDecls globals
  (Env vs, main') <- local (const env) (analyzeMain main)
  let vs' = fmap (\(n, t) -> Typed t n) (Map.toList vs)
  return (B.boogie vs' main')

analyzeMain :: AST.Main -> Analyze (Env Name, B.Main)
analyzeMain (AST.Main modifies pre returns locals toplevel post) = do
  pre' <- mapM analyzeProperty pre

  env <- analyzeDecls locals
  env' <- lift . foldM extendEnv env $ case returns of
    Just (AST.Returns r) -> NE.toList r
    Nothing -> []

  toplevel' <- local (const env') (analyzeTopLevels toplevel)
  post' <- local (const env') (mapM analyzeProperty post)

  return (env', B.main (A.astValue <$> modifies) pre' toplevel' post')

analyzeDecls :: [AST.Decl] -> Analyze (Env Name)
analyzeDecls ds = do
  env <- ask
  foldM (\e d -> local (const e) (analyzeDecl d)) env ds

analyzeDecl :: AST.Decl -> Analyze (Env Name)
analyzeDecl (AST.Declare ns) = do
  env <- ask
  lift $ foldM extendEnv env (sequence ns)

guardType :: (TypeOf b, Pretty b)
          => (a -> Analyze b) -> Type -> A.AST a -> Analyze b
guardType analyze t (A.AST pos a) = do
  b <- analyze a
  let t' = typeOf b
  lift $ if t == t'
         then Right b
         else Left (TypeMismatch (A.AST pos (Typed t' b)) t)

infix 6 <:$>
(<:$>) :: (TypeOf b, Pretty b)
       => (a -> Analyze b) -> A.AST a -> Type -> Analyze b
f <:$> a = \t -> guardType f t a

infix 6 `guardAll`
guardAll :: (TypeOf b, Pretty b)
         => (a -> Analyze b) -> NonEmpty (A.AST a) -> NonEmpty Type
         -> Analyze (NonEmpty b)
guardAll f as ts = VNE.zipWithM (guardType f) ts as

infix 5 .:
(.:) :: (a -> b) -> a -> b
(.:) = ($)

analyzeTopLevels :: [Either AST.Stmt AST.Assume] -> Analyze [B.TopLevel]
analyzeTopLevels = mapMaybeM analyzeTopLevel

analyzeTopLevel :: Either AST.Stmt AST.Assume -> Analyze (Maybe B.TopLevel)
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
  env <- ask
  var <- lift (lookupEnv ast env)
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
    localQV <- lift $ foldM extendEnv emptyEnv flatVars
    let qv' = qv <> localQV
    f' <- analyzeFormula qv' f
    let vars' = fmap (fmap A.astValue) flatVars
    return (F.quantify q vars' f')

analyzeName :: QV -> A.AST Name -> Analyze (A.AST F.Term)
analyzeName qv ast = do
  env <- ask
  let analyzeVar = fmap F.variable <$> lookupEnv (F.var <$> ast) qv
  let analyzeSym = fmap F.constant <$> lookupEnv ast env
  lift $ analyzeVar <> analyzeSym

analyzeSelect :: QV -> A.AST F.Term -> NonEmpty F.AST.Term -> Analyze (A.AST F.Term)
analyzeSelect qv ast@(A.AST pos term) as = case typeOf term of
  Array ts _ -> A.AST pos <$> (F.select term <$> analyzeTerm qv `guardAll` as .: ts)
  t -> lift $ Left (NonArraySelect (Typed t <$> ast))
