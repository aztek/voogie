module Voogie.Front (analyze) where

import Control.Monad
import Control.Monad.Extra
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

data Env' a = Env (Map a Type)

instance Ord a => Semigroup (Env' a) where
  Env m1 <> Env m2 = Env (m1 <> m2)

emptyEnv :: (Ord a, Named a) => Env' a
emptyEnv = Env Map.empty

lookupEnv :: (Ord a, Named a) => A.AST a -> Env' a -> Result (A.AST (Typed a))
lookupEnv ast (Env vs)
  | Just t <- Map.lookup (A.astValue ast) vs = Right (Typed t <$> ast)
  | otherwise = Left (UndefinedVariable ast)

insertEnv :: (Ord a, Named a) => Typed a -> Env' a -> Env' a
insertEnv (Typed t n) (Env vs) = Env (Map.insert n t vs)

extendEnv :: (Ord a, Named a) => Env' a -> Typed (A.AST a) -> Result (Env' a)
extendEnv env (Typed t ast@(A.AST pos n))
  | Left _ <- lookupEnv ast env = Right (insertEnv var env)
  | otherwise = Left (MultipleDefinitions (A.AST pos var))
  where var = Typed t n

type Env = Env' Name

analyze :: AST.Boogie -> Result Boogie
analyze (AST.Boogie globals main) = do
  env <- foldM analyzeDecl emptyEnv globals
  (Env vs, main') <- analyzeMain env main
  let vs' = fmap (\(n, t) -> Typed t n) (Map.toList vs)
  return (B.boogie vs' main')

analyzeMain :: Env -> AST.Main -> Result (Env, B.Main)
analyzeMain env (AST.Main modifies pre returns locals toplevel post) = do
  env'  <- foldM analyzeDecl env locals
  env'' <- foldM extendEnv env' $ case returns of
    Just (AST.Returns r) -> NE.toList r
    Nothing -> []
  pre' <- mapM (analyzeProperty env) pre
  toplevel' <- analyzeTopLevels env'' toplevel
  post' <- mapM (analyzeProperty env'') post
  return (env'', B.main (A.astValue <$> modifies) pre' toplevel' post')

analyzeDecl :: Env -> AST.Decl -> Result Env
analyzeDecl env (AST.Declare ns) = foldM extendEnv env (sequence ns)

guardType :: (TypeOf b, Pretty b)
          => (a -> Result b) -> Type -> A.AST a -> Result b
guardType analyze t (A.AST pos a) = do
  b <- analyze a
  let t' = typeOf b
  if t == t'
  then Right b
  else Left (TypeMismatch (A.AST pos (Typed t' b)) t)

infix 6 <:$>
(<:$>) :: (TypeOf b, Pretty b)
       => (a -> Result b) -> A.AST a -> Type -> Result b
f <:$> a = \t -> guardType f t a

infix 6 `guardAll`
guardAll :: (TypeOf b, Pretty b)
         => (a -> Result b) -> NonEmpty (A.AST a) -> NonEmpty Type
         -> Result (NonEmpty b)
guardAll f as ts = VNE.zipWithM (guardType f) ts as

infix 5 .:
(.:) :: (a -> b) -> a -> b
(.:) = ($)

analyzeTopLevels :: Env -> [Either AST.Stmt AST.Assume] -> Result [B.TopLevel]
analyzeTopLevels env = mapMaybeM (analyzeTopLevel env)

analyzeTopLevel :: Env -> Either AST.Stmt AST.Assume -> Result (Maybe B.TopLevel)
analyzeTopLevel env = \case
  Left stmt -> fmap Left <$> analyzeStmt env (A.astValue stmt)
  Right assume -> Just . Right <$> analyzeAssume env assume

analyzeStmts :: Env -> [AST.Stmt] -> Result [B.Statement]
analyzeStmts env = fmap catMaybes . mapM (analyzeStmt env . A.astValue)

analyzeStmt :: Env -> AST.Stmt' -> Result (Maybe B.Statement)
analyzeStmt env = \case
  AST.If c a b -> B.if_ <$> analyzeExpr env <:$> c .: Boolean
                        <*> analyzeStmts env a <*> analyzeStmts env b
  AST.Assign ass -> B.assign <$> mapM (analyzeAssignment env) ass

analyzeAssume :: Env -> AST.Assume -> Result B.Assume
analyzeAssume env (AST.Assume f) = B.assume <$> analyzeProperty env f

analyzeAssignment :: Env -> (AST.LVal, AST.Expr) -> Result B.Assignment
analyzeAssignment env (lval, e) = do
  lv <- analyzeLValue env lval
  e' <- analyzeExpr env <:$> e .: typeOf lv
  return (lv, e')

analyzeLValue :: Env -> AST.LVal -> Result B.LValue
analyzeLValue env (AST.Ref ast is) = do
  var <- lookupEnv ast env
  let n = A.astValue var
  let t = typeOf n
  let ais = arrayIndexes t
  xs <- if length is > length ais
        then Left (ArrayDimensionMismatch (Typed t <$> var))
        else Right (zip is ais)
  B.lvalue n <$> mapM (uncurry $ guardAll (analyzeExpr env)) xs

analyzeExpr :: Env -> AST.Expr' -> Result B.Expression
analyzeExpr env = \case
  AST.IntConst  i -> return (B.integerLiteral i)
  AST.BoolConst b -> return (B.booleanLiteral b)
  AST.LVal lval -> do
    lval' <- analyzeLValue env lval
    return (B.ref lval')
  AST.Unary op e -> do
    let d = unaryOpDomain op
    e' <- analyzeExpr env <:$> e .: d
    return (B.unary op e')
  AST.Binary op a b -> do
    let (d1, d2) = binaryOpDomain op
    a' <- analyzeExpr env <:$> a .: d1
    b' <- analyzeExpr env <:$> b .: d2
    return (B.binary op a' b')
  AST.Equals s a b -> do
    a' <- analyzeExpr env (A.astValue a)
    b' <- analyzeExpr env <:$> b .: typeOf a'
    return (B.equals s a' b')
  AST.Ternary c a b -> do
    c' <- analyzeExpr env <:$> c .: Boolean
    a' <- analyzeExpr env (A.astValue a)
    b' <- analyzeExpr env <:$> b .: typeOf a'
    return (B.ifElse c' a' b')

analyzeProperty :: Env -> F.AST.Term -> Result F.Formula
analyzeProperty = analyzeFormula emptyEnv

type QV = Env' F.Var

analyzeFormula :: QV -> Env -> F.AST.Term -> Result F.Formula
analyzeFormula qv env f = analyzeTerm qv env <:$> f .: Boolean

analyzeTerm :: QV -> Env -> F.AST.Term' -> Result F.Term
analyzeTerm qv env = \case
  F.AST.IntConst  i -> return (F.integerConstant i)
  F.AST.BoolConst b -> return (F.booleanConstant b)
  F.AST.Ref ast is -> do
    var <- analyzeName qv env ast
    A.astValue <$> foldM (analyzeSelect qv env) var is
  F.AST.Unary op t -> do
    let d = unaryOpDomain op
    t' <- analyzeTerm qv env <:$> t .: d
    return (F.unary op t')
  F.AST.Binary op a b -> do
    let (d1, d2) = binaryOpDomain op
    a' <- analyzeTerm qv env <:$> a .: d1
    b' <- analyzeTerm qv env <:$> b .: d2
    return (F.binary op a' b')
  F.AST.Ternary c a b -> do
    c' <- analyzeFormula qv env c
    a' <- analyzeTerm qv env (A.astValue a)
    b' <- analyzeTerm qv env <:$> b .: typeOf a'
    return (F.if_ c' a' b')
  F.AST.Equals s a b -> do
    a' <- analyzeTerm qv env (A.astValue a)
    b' <- analyzeTerm qv env <:$> b .: typeOf a'
    return (F.equals s a' b')
  F.AST.Quantified q vars f -> do
    let flatVars = fmap (fmap F.var <$>) (sequence =<< vars)
    localQV <- foldM extendEnv emptyEnv flatVars
    let qv' = qv <> localQV
    f' <- analyzeFormula qv' env f
    let vars' = fmap (fmap A.astValue) flatVars
    return (F.quantify q vars' f')

analyzeName :: QV -> Env -> A.AST Name -> Result (A.AST F.Term)
analyzeName qv env ast =
     (fmap F.variable <$> lookupEnv (F.var <$> ast) qv)
  <> (fmap F.constant <$> lookupEnv ast env)

analyzeSelect :: QV -> Env -> A.AST F.Term -> NonEmpty F.AST.Term -> Result (A.AST F.Term)
analyzeSelect qv env ast@(A.AST pos term) as = case typeOf term of
  Array ts _ -> A.AST pos <$> (F.select term <$> analyzeTerm qv env `guardAll` as .: ts)
  t -> Left (NonArraySelect (Typed t <$> ast))
