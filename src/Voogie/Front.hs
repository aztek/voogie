module Voogie.Front (analyze) where

import Control.Monad
import Control.Monad.Extra
import Data.Maybe

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Voogie.NonEmpty as VNE

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

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

newtype Env = Env (Map Name Type)

emptyEnv :: Env
emptyEnv = Env Map.empty

lookupVariable :: A.AST Name -> Env -> Result (A.AST (Typed Name))
lookupVariable ast (Env vs)
  | Just t <- Map.lookup (A.astValue ast) vs = Right (Typed t <$> ast)
  | otherwise = Left (UndefinedVariable ast)

insertVariable :: Typed Name -> Env -> Env
insertVariable (Typed t n) (Env vs) = Env (Map.insert n t vs)

extendEnv :: Env -> Typed (A.AST Name) -> Result Env
extendEnv env (Typed t ast@(A.AST pos n))
  | Left _ <- lookupVariable ast env = Right (insertVariable var env)
  | otherwise = Left (MultipleDefinitions (A.AST pos var))
  where var = Typed t n

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
analyzeTopLevels env = mapMaybeM analyzeTopLevel
  where
    analyzeTopLevel :: Either AST.Stmt AST.Assume -> Result (Maybe B.TopLevel)
    analyzeTopLevel (Left stmt) = fmap Left <$> analyzeStmt (A.astValue stmt)
    analyzeTopLevel (Right assume) = Just . Right <$> analyzeAssume assume

    analyzeStmts :: [AST.Stmt] -> Result [B.Statement]
    analyzeStmts = fmap catMaybes . mapM (analyzeStmt . A.astValue)

    analyzeStmt :: AST.Stmt' -> Result (Maybe B.Statement)
    analyzeStmt = \case
      AST.If c a b -> B.if_ <$> analyzeExpr <:$> c .: Boolean
                            <*> analyzeStmts a <*> analyzeStmts b
      AST.Assign ass -> B.assign <$> mapM analyzeAssignment ass

    analyzeAssume :: AST.Assume -> Result B.Assume
    analyzeAssume (AST.Assume f) = B.assume <$> analyzeProperty env f

    analyzeAssignment :: (AST.LVal, AST.Expr) -> Result B.Assignment
    analyzeAssignment (lval, e) = do
      lv <- analyzeLValue lval
      e' <- analyzeExpr <:$> e .: typeOf lv
      return (lv, e')

    analyzeLValue :: AST.LVal -> Result B.LValue
    analyzeLValue (AST.Ref ast is) = do
      var <- lookupVariable ast env
      let n = A.astValue var
      let t = typeOf n
      let ais = arrayIndexes t
      xs <- if length is > length ais
            then Left (ArrayDimensionMismatch (Typed t <$> var))
            else Right (zip is ais)
      B.lvalue n <$> mapM (uncurry $ guardAll analyzeExpr) xs

    analyzeExpr :: AST.Expr' -> Result B.Expression
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

analyzeProperty :: Env -> F.AST.Term -> Result F.Formula
analyzeProperty env = analyzeFormula (env, Set.empty)

type Context = (Env, Set (Typed Name))

extendContext :: Typed Name -> Context -> Context
extendContext v (env, qv) = (insertVariable v env, Set.insert v qv)

analyzeFormula :: Context -> F.AST.Term -> Result F.Formula
analyzeFormula ctx@(env, qv) f = analyzeTerm <:$> f .: Boolean
  where
    analyzeTerm :: F.AST.Term' -> Result F.Term
    analyzeTerm = \case
      F.AST.IntConst  i -> return (F.integerConstant i)
      F.AST.BoolConst b -> return (F.booleanConstant b)
      F.AST.Ref ast is -> do
        var <- analyzeVar ast
        A.astValue <$> foldM analyzeSelect var is
      F.AST.Unary op t -> do
        let d = unaryOpDomain op
        t' <- analyzeTerm <:$> t .: d
        return (F.unary op t')
      F.AST.Binary op a b -> do
        let (d1, d2) = binaryOpDomain op
        a' <- analyzeTerm <:$> a .: d1
        b' <- analyzeTerm <:$> b .: d2
        return (F.binary op a' b')
      F.AST.Ternary c a b -> do
        c' <- analyzeFormula ctx c
        a' <- analyzeTerm (A.astValue a)
        b' <- analyzeTerm <:$> b .: typeOf a'
        return (F.if_ c' a' b')
      F.AST.Equals s a b -> do
        a' <- analyzeTerm (A.astValue a)
        b' <- analyzeTerm <:$> b .: typeOf a'
        return (F.equals s a' b')
      F.AST.Quantified q vars f -> do
        (ctx', vars') <- analyzeVarList vars
        f' <- analyzeFormula ctx' f
        return (F.quantify q vars' f')

    analyzeVar :: A.AST Name -> Result (A.AST F.Term)
    analyzeVar ast = do
      var <- lookupVariable ast env
      let analyzeVar' v = if v `Set.member` qv
                          then F.variable (F.var <$> v)
                          else F.constant v
      return (analyzeVar' <$> var)

    analyzeSelect :: A.AST F.Term -> NonEmpty F.AST.Term -> Result (A.AST F.Term)
    analyzeSelect ast@(A.AST pos term) as = case typeOf term of
      Array ts _ -> A.AST pos <$> (F.select term <$> analyzeTerm `guardAll` as .: ts)
      t -> Left (NonArraySelect (Typed t <$> ast))

    analyzeVarList :: F.AST.VarList -> Result (Context, F.VarList)
    -- TODO: check that the variables are disjoint
    analyzeVarList vars = Right (ctx', fmap (fmap F.var) vars'')
      where
        vars' = fmap (fmap $ fmap A.astValue) vars
        vars'' = sequence =<< vars'
        ctx' = foldr extendContext ctx vars''
