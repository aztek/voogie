{-#LANGUAGE GADTs #-}

module Kyckling.Program where

data Type = Int | Bool | IA | BA

data IntArray
data BoolArray

data Connective = And | Or | Iff

instance Show Connective where
  show And = "&&"
  show Or  = "||"
  show Iff = "=="

data Comparison = Eq | Ineq | Geq | Leq | G | L

instance Show Comparison where
  show Eq   = "=="
  show Ineq = "!="
  show Geq  = ">="
  show Leq  = "<="
  show G    = ">"
  show L    = "<"

data ArithOp = Plus | Minus

instance Show ArithOp where
  show Plus  = "+"
  show Minus = "-"

type Var = String
type Env = [(Var, Type)]

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool

  Not :: Expr Bool -> Expr Bool
  Conn :: Connective -> Expr Bool -> Expr Bool -> Expr Bool
  Compare :: Comparison -> Expr Int  -> Expr Int  -> Expr Bool

  Op :: ArithOp -> Expr Int -> Expr Int -> Expr Int

  SelectI :: Var -> Expr Int -> Expr Int
  SelectB :: Var -> Expr Int -> Expr Bool

  VarI  :: Var -> Expr Int
  VarB  :: Var -> Expr Bool
  VarIA :: Var -> Expr Int
  VarBA :: Var -> Expr Bool

  Tern :: Expr Bool -> Expr a -> Expr a -> Expr a

instance Show (Expr a) where
  show (I i) = show i
  show (B True) = "true"
  show (B False) = "false"
  show (Not e) = "!" ++ show e
  show (Conn c e1 e2) = show e1 ++ " " ++ show c ++ " " ++ show e2
  show (Compare c e1 e2) = show e1 ++ " " ++ show c ++ " " ++ show e2
  show (Op o e1 e2) = show e1 ++ " " ++ show o ++ " " ++ show e2
  show (SelectI a i) = show a ++ "[" ++ show i ++ "]"
  show (SelectB a i) = show a ++ "[" ++ show i ++ "]"
  show (VarI  v) = v
  show (VarB  v) = v
  show (VarIA v) = v
  show (VarBA v) = v
  show (Tern c a b) = show c ++ " ? " ++ show a ++ " : " ++ show b

data Program where
  IfElse :: Expr Bool -> Program -> Program -> Program
  If :: Expr Bool -> Program -> Program
  (:=) :: Var -> Expr a -> Program
  Compose :: Program -> Program -> Program

toIndent :: Int -> String
toIndent n = replicate (n * 2) ' '

showIndented :: Int -> Program -> String
showIndented n (If c a) = toIndent n ++ "if (" ++ show c ++ ") {" ++ "\n" ++
                                          showIndented (n + 1) a ++
                          toIndent n ++ "}" ++ "\n"
showIndented n (IfElse c a b) = toIndent n ++ "if (" ++ show c ++ ") {" ++ "\n" ++
                                                showIndented (n + 1) a ++
                                toIndent n ++ "} else {" ++ "\n" ++
                                                showIndented (n + 1) b ++
                                              "}" ++ "\n"
showIndented n (v := e) = toIndent n ++ v ++ " = " ++ show e ++ ";" ++ "\n"
showIndented n (Compose p1 p2) = showIndented n p1 ++ showIndented n p2

instance Show Program where
  show = showIndented 0