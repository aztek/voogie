module Voogie.Front where

import Control.Monad (zipWithM, liftM)
import Control.Monad.Extra (mapMaybeM)
import Control.Applicative
import Data.Maybe
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Voogie.Theory
import Voogie.Pretty
import Voogie.Boogie
import Voogie.Boogie.Pretty
import qualified Voogie.Boogie.AST as AST

import qualified Voogie.FOOL.Smart as F
import qualified Voogie.FOOL.AST as F.AST
import qualified Voogie.FOOL.Pretty as F.P

type Error = String

data FunType = FunType [Type] Type

data Env = Env (Map Name Type)

emptyEnv :: Env
emptyEnv = Env Map.empty

lookupVariable :: Name -> Env -> Either Error (Typed Name)
lookupVariable name (Env vs) = case Map.lookup name vs of
                                 Nothing  -> Left  ("undefined variable " ++ name)
                                 Just typ -> Right (Typed name typ)

lookupArrayName :: Name -> Env -> Either Error (Typed Name)
lookupArrayName name env = do name <- lookupVariable name env
                              case typeOf name of
                                Array _ _ -> Right name
                                otherwise -> Left "not an array"

insertVariable :: Typed Name -> Env -> Env
insertVariable (Typed n t) (Env vs) = Env (Map.insert n t vs)

variables :: Env -> [Typed Name]
variables (Env vs) = map (uncurry Typed) (Map.toList vs)

analyze :: AST.AST -> Either Error Boogie
analyze (AST.AST globalDecls (AST.Main pre r mainDecls ss post)) =
  do globalEnv <- foldrM analyzeDecl emptyEnv  globalDecls
     mainEnv'  <- foldrM analyzeDecl globalEnv mainDecls
     let mainEnv = case r of
                     Just (AST.Returns r) -> insertVariable r mainEnv'
                     Nothing -> mainEnv'
     ss'   <- analyzeMain mainEnv ss
     pre'  <- mapM (analyzeFormula globalEnv) pre
     post' <- mapM (analyzeFormula mainEnv)   post
     let main = Main pre' ss' post'
     let vars = variables mainEnv
     return (Boogie vars main)

analyzeDecl :: AST.Decl -> Env -> Either Error Env
analyzeDecl (AST.Declare (Typed ns t)) env = foldrM insertVariable' env ns
  where
    insertVariable' :: Name -> Env -> Either Error Env
    insertVariable' n env =
      case lookupVariable n env of
        Left  _ -> Right (insertVariable (Typed n t) env)
        Right _ -> Left  ("redefined variable " ++ n)


analyzeMain :: Env -> [Either AST.Stmt AST.Assume] -> Either Error [Either Statement Assume]
analyzeMain env = mapMaybeM $ either (fmap (fmap   Left)  . analyzeStmt   env)
                                     (fmap (Just . Right) . analyzeAssume env)


guardType :: (TypeOf b, Pretty b) => (a -> Either Error b) -> Type -> a -> Either Error b
guardType analyze t a = do b <- analyze a
                           let t' = typeOf b
                           if t == t' then return b else
                             Left $ "expected an expression of the type " ++ pretty t' ++
                                    " but got " ++ pretty b ++ " of the type " ++ pretty t 

guardTypes :: (TypeOf b, Pretty b) => (a -> Either Error b) -> [Type] -> [a] -> Either Error [b]
guardTypes analyze = zipWithM (guardType analyze)

infix 6 `guard`
guard :: (TypeOf b, Pretty b) => (a -> Either Error b) -> a -> Type -> Either Error b
guard f = flip (guardType f)

infix 6 `guardAll`
guardAll :: (TypeOf b, Pretty b) => (a -> Either Error b) -> [a] -> [Type] -> Either Error [b]
guardAll f = flip (guardTypes f)

infix 5 .:
(.:) :: (a -> b) -> a -> b
(.:) = ($)


analyzeStmts :: Env -> [AST.Stmt] -> Either Error [Statement]
analyzeStmts env = fmap catMaybes . mapM (analyzeStmt env)

analyzeStmt :: Env -> AST.Stmt -> Either Error (Maybe Statement)
analyzeStmt env (AST.If c a b) =
  do c' <- analyzeExpr  env c
     a' <- analyzeStmts env a
     b' <- analyzeStmts env b
     return $ case NE.nonEmpty a' of
                Just a' -> Just (If c' False a' b')
                Nothing -> case NE.nonEmpty b' of
                             Nothing -> Nothing
                             Just b' -> Just (If c' True b' [])
analyzeStmt _   (AST.Assign lvals rvals) | length lvals /= length rvals = Left ""
analyzeStmt env (AST.Assign lvals rvals) =
  do ass <- mapM (analyzeAssignment env) (NE.zip lvals rvals)
     return $ Just (Assign ass)

analyzeAssume :: Env -> AST.Assume -> Either Error Assume
analyzeAssume env (AST.Assume f) = Assume <$> analyzeFormula env f

analyzeAssignment :: Env -> (AST.LVal, AST.Expr) -> Either Error (LValue, Expression)
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
analyzeTerm (env, qv) (F.AST.Const c) =
  do var <- lookupVariable c env
     return $ if Set.member var qv
              then F.variable (fmap F.var var)
              else F.constant var
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
analyzeTerm (env, qv) (F.AST.Quantified q vars term) = F.quantify q vars' <$> analyzeFormula' (env', qv') term
  where
    -- TODO: check that the variables are disjoint
    env' = foldr insertVariable env vars
    qv'  = foldr Set.insert     qv  vars
    vars' = fmap (fmap F.var) vars
analyzeTerm ctx@(env, _) (F.AST.ArrayElem s i) = F.select <$> array <*> index
  where
    array = F.constant <$> lookupArrayName s env
    index = analyzeTerm ctx `guard` i .: Integer

analyzeLValue :: Env -> AST.LVal -> Either Error LValue
analyzeLValue env (AST.Var s) = Variable <$> lookupVariable s env
analyzeLValue env (AST.ArrayElem s i) = ArrayElem <$> array <*> index
  where
    array = lookupArrayName s env
    index = analyzeExpr env `guard` i .: Integer

analyzeExpr :: Env -> AST.Expr -> Either Error Expression
analyzeExpr _ (AST.IntConst  i) = return (IntegerLiteral i)
analyzeExpr _ (AST.BoolConst b) = return (BooleanLiteral b)
analyzeExpr env (AST.LVal lval) = Ref <$> analyzeLValue env lval
analyzeExpr env (AST.Unary op e) = Unary op <$> analyzeExpr env `guard` e .: d
  where
    d = unaryOpDomain op
analyzeExpr env (AST.Binary op a b) = Binary op <$> a' <*> b'
  where
    a' = analyzeExpr env `guard` a .: d1
    b' = analyzeExpr env `guard` b .: d2
    (d1, d2) = binaryOpDomain op
analyzeExpr env (AST.Equals s a b) =
  do a' <- analyzeExpr env a
     b' <- analyzeExpr env `guard` b .: typeOf a'
     return (Equals s a' b')
analyzeExpr env (AST.Ternary c a b) =
  do c' <- analyzeExpr env `guard` c .: Boolean
     a' <- analyzeExpr env a
     b' <- analyzeExpr env `guard` b .: typeOf a'
     return (IfElse c' a' b')
