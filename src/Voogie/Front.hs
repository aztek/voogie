module Voogie.Front where

import Control.Monad (zipWithM, liftM, foldM, join)
import Control.Monad.Extra (mapMaybeM)
import Control.Applicative
import Data.Maybe
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
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
import Voogie.Boogie.Pretty

import qualified Voogie.FOOL.Smart as F
import qualified Voogie.FOOL.AST as F.AST
import Voogie.FOOL.Pretty

type Error = String

data FunType = FunType [Type] Type

data Env = Env (Map Name Type)

emptyEnv :: Env
emptyEnv = Env Map.empty

lookupVariable :: Name -> Env -> Either Error (Typed Name)
lookupVariable name (Env vs) = case Map.lookup name vs of
                                 Nothing  -> Left  ("undefined variable " ++ name)
                                 Just typ -> Right (Typed name typ)

insertVariable :: Typed Name -> Env -> Env
insertVariable (Typed n t) (Env vs) = Env (Map.insert n t vs)

variables :: Env -> [Typed Name]
variables (Env vs) = map (uncurry Typed) (Map.toList vs)

analyze :: AST.AST -> Either Error Boogie
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

analyzeDecl :: AST.Decl -> Env -> Either Error Env
analyzeDecl (AST.Declare (Typed ns t)) env = foldrM insertVariable' env ns
  where
    insertVariable' :: Name -> Env -> Either Error Env
    insertVariable' n env =
      case lookupVariable n env of
        Left  _ -> Right (insertVariable (Typed n t) env)
        Right _ -> Left  ("redefined variable " ++ n)


analyzeMain :: Env -> [Either AST.Stmt AST.Assume] -> Either Error [Either B.Statement B.Assume]
analyzeMain env = mapMaybeM $ either (fmap (fmap   Left)  . analyzeStmt   env)
                                     (fmap (Just . Right) . analyzeAssume env)


guardType :: (TypeOf b, Pretty b) => (a -> Either Error b) -> Type -> a -> Either Error b
guardType analyze t a = do b <- analyze a
                           let t' = typeOf b
                           if t == t' then return b else
                             Left $ "expected an expression of the type " ++ pretty t ++
                                    " but got " ++ pretty b ++ " of the type " ++ pretty t'

guardTypes :: (TypeOf b, Pretty b) => (a -> Either Error b) -> NonEmpty Type -> NonEmpty a -> Either Error (NonEmpty b)
guardTypes analyze = VNE.zipWithM (guardType analyze)

infix 6 `guard`
guard :: (TypeOf b, Pretty b) => (a -> Either Error b) -> a -> Type -> Either Error b
guard f = flip (guardType f)

infix 6 `guardAll`
guardAll :: (TypeOf b, Pretty b) => (a -> Either Error b) -> NonEmpty a -> NonEmpty Type -> Either Error (NonEmpty b)
guardAll f = flip (guardTypes f)

infix 5 .:
(.:) :: (a -> b) -> a -> b
(.:) = ($)


analyzeStmts :: Env -> [AST.Stmt] -> Either Error [B.Statement]
analyzeStmts env = fmap catMaybes . mapM (analyzeStmt env)

analyzeStmt :: Env -> AST.Stmt -> Either Error (Maybe B.Statement)
analyzeStmt env (AST.If c a b) =
  do c' <- analyzeExpr  env c
     a' <- analyzeStmts env a
     b' <- analyzeStmts env b
     return (B.if_ c' a' b')
analyzeStmt _   (AST.Assign lvals rvals) | length lvals /= length rvals = Left ""
analyzeStmt env (AST.Assign lvals rvals) =
  do ass <- mapM (analyzeAssignment env) (NE.zip lvals rvals)
     return (B.assign ass)

analyzeAssume :: Env -> AST.Assume -> Either Error B.Assume
analyzeAssume env (AST.Assume f) = B.assume <$> analyzeFormula env f

analyzeAssignment :: Env -> (AST.LVal, AST.Expr) -> Either Error (B.LValue, B.Expression)
analyzeAssignment env (lval, e) =
  do lv <- analyzeLValue env lval
     e' <- analyzeExpr env `guard` e .: typeOf lv
     return (lv, e')

analyzeFormula :: Env -> F.AST.Term -> Either Error F.Formula
analyzeFormula env = analyzeFormula' (env, Set.empty)

analyzeFormula' :: (Env, Set (Typed Name)) -> F.AST.Term -> Either Error F.Formula
analyzeFormula' ctx f = analyzeTerm ctx `guard` f .: Boolean

analyzeTerm :: (Env, Set (Typed Name)) -> F.AST.Term -> Either Error F.Term
analyzeTerm _ (F.AST.IntConst  i) = return (F.integerConstant i)
analyzeTerm _ (F.AST.BoolConst b) = return (F.booleanConstant b)
analyzeTerm ctx@(env, qv) (F.AST.Ref n is) =
  do var <- lookupVariable n env
     foldM analyzeIndex (analyzeVar var) is
  where
    analyzeVar :: Typed Name -> F.Term
    analyzeVar v = if Set.member v qv then F.variable (fmap F.var v) else F.constant v

    analyzeIndex :: F.Term -> NonEmpty F.AST.Term -> Either Error F.Term
    analyzeIndex term as = case typeOf term of
      Array ts _ -> F.select term <$> analyzeTerm ctx `guardAll` as .: ts
      t -> Left $ "expected an expression of an array type," ++
                  " but got " ++ pretty term ++ " of the type " ++ pretty t
analyzeTerm ctx (F.AST.Unary op t) = F.unary op <$> analyzeTerm ctx `guard` t .: d
  where
    d = unaryOpDomain op
analyzeTerm ctx (F.AST.Binary op a b) = F.binary op <$> a' <*> b'
  where
    a' = analyzeTerm ctx `guard` a .: d1
    b' = analyzeTerm ctx `guard` b .: d2
    (d1, d2) = binaryOpDomain op
analyzeTerm ctx (F.AST.Ternary c a b) =
  do c' <- analyzeFormula' ctx c
     a' <- analyzeTerm ctx a
     b' <- analyzeTerm ctx `guard` b .: typeOf a'
     return (F.if_ c' a' b')
analyzeTerm ctx (F.AST.Equals s a b) =
  do a' <- analyzeTerm ctx a
     b' <- analyzeTerm ctx `guard` b .: typeOf a'
     return (F.equals s a' b')
analyzeTerm (env, qv) (F.AST.Quantified q vars term) =
  do ids <- analyzeVarList vars
     let env' = foldr insertVariable env ids
     let qv'  = foldr Set.insert     qv  ids
     f <- analyzeFormula' (env', qv') term
     let vars' = fmap (fmap F.var) ids
     return (F.quantify q vars' f)

analyzeVarList :: NonEmpty (Typed (NonEmpty String)) -> Either Error (NonEmpty (Typed Name))
-- TODO: check that the variables are disjoint
analyzeVarList = Right . join . fmap propagate
  where
    propagate :: Typed (NonEmpty a) -> NonEmpty (Typed a)
    propagate (Typed a t) = fmap (flip Typed t) a

analyzeLValue :: Env -> AST.LVal -> Either Error B.LValue
analyzeLValue env (AST.Ref n is) =
  do var <- lookupVariable n env
     is' <- analyzeIndexes (typeOf var) is
     return (B.lvalue var is')
  where
    analyzeIndexes :: Type -> [NonEmpty AST.Expr] -> Either Error [NonEmpty B.Expression]
    analyzeIndexes _ [] = Right []
    analyzeIndexes (Array ts r) (i:is) | length i <= length ts =
      do i' <- analyzeExpr env `guardAll` i .: ts
         is' <- analyzeIndexes r is
         return (i' : is')
    analyzeIndexes t is = Left $ "expected an expression of an array type," ++
                                 " but got " ++ n ++ " of the type " ++ pretty t


analyzeExpr :: Env -> AST.Expr -> Either Error B.Expression
analyzeExpr _ (AST.IntConst  i) = return (B.integerLiteral i)
analyzeExpr _ (AST.BoolConst b) = return (B.booleanLiteral b)
analyzeExpr env (AST.LVal lval) = B.ref <$> analyzeLValue env lval
analyzeExpr env (AST.Unary op e) =
  do let d = unaryOpDomain op
     e' <- analyzeExpr env `guard` e .: d
     return (B.unary op e')
analyzeExpr env (AST.Binary op a b) =
  do let (d1, d2) = binaryOpDomain op
     a' <- analyzeExpr env `guard` a .: d1
     b' <- analyzeExpr env `guard` b .: d2
     return (B.binary op a' b')
analyzeExpr env (AST.Equals s a b) =
  do a' <- analyzeExpr env a
     b' <- analyzeExpr env `guard` b .: typeOf a'
     return (B.equals s a' b')
analyzeExpr env (AST.Ternary c a b) =
  do c' <- analyzeExpr env `guard` c .: Boolean
     a' <- analyzeExpr env a
     b' <- analyzeExpr env `guard` b .: typeOf a'
     return (B.ifElse c' a' b')
