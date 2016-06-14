{-#LANGUAGE GADTs,DataKinds,KindSignatures,ExistentialQuantification #-}

module Kyckling.Program where

import Text.Printf
import Data.List

data Type = I | B | A Type

type Var = String
type Env = [(Var, Type)]

data UnaryOp (a :: Type) (b :: Type) where
  Negate   :: UnaryOp B B
  Positive :: UnaryOp I I
  Negative :: UnaryOp I I

data BinaryOp (a :: Type) (b :: Type) (c :: Type) where
  And      :: BinaryOp B B B
  Or       :: BinaryOp B B B

  Greater  :: BinaryOp I I B
  Less     :: BinaryOp I I B
  Geq      :: BinaryOp I I B
  Leq      :: BinaryOp I I B

  Add      :: BinaryOp I I I
  Subtract :: BinaryOp I I I
  Multiply :: BinaryOp I I I

  Select   :: BinaryOp (A b) I b

  Eq       :: BinaryOp a a B
  InEq     :: BinaryOp a a B

data TernaryOp (a :: Type) (b :: Type) (c :: Type) (d :: Type) where
  IfElse :: TernaryOp B a a a

data Expression (t :: Type) where
  LiftInt  :: Integer -> Expression I
  LiftBool :: Bool -> Expression B

  Unary   :: UnaryOp   a b     -> Expression a -> Expression b
  Binary  :: BinaryOp  a b c   -> Expression a -> Expression b -> Expression c
  Ternary :: TernaryOp a b c d -> Expression a -> Expression b -> Expression c -> Expression d

  Var :: Var -> Type -> Expression t

data Statement = forall t. Declare Type Var (Maybe (Expression t))
               | forall t. Assign Var (Expression t)
               | If (Expression B) [Statement] [Statement]
               | Assert (Expression B)

data Program = Program [Statement]

instance Show Type where
  show I = "int"
  show B = "bool"
  show (A t) = show t ++ "[]"

instance Show (UnaryOp a b) where
  show Negate   = "!"
  show Positive = "+"
  show Negative = "-"

instance Show (BinaryOp a b c) where
  show And = "&&"
  show Or  = "||"

  show Greater = ">"
  show Less    = "<"
  show Geq     = ">="
  show Leq     = "<="

  show Add      = "+"
  show Subtract = "-"
  show Multiply = "*"

  show Select   = "[]"

  show Eq       = "=="
  show InEq     = "!="

instance Show (Expression a) where
  show (LiftInt i) = show i
  show (LiftBool True)  = "true"
  show (LiftBool False) = "false"

  show (Unary op e) = show e ++ show op
  show (Binary Select a b) = show a ++ "[" ++ show b ++ "]"
  show (Binary op a b) = show a ++ show op ++ show b
  show (Ternary IfElse a b c) = show a ++ " ? " ++ show a ++ " : " ++ show b

  show (Var v _) = v

toIndent :: Int -> String
toIndent n = replicate (n * 2) ' '

els :: Int -> String
els n = toIndent n ++ "else" ++ "\n"

lbra :: Int -> String
lbra n = toIndent n ++ "{" ++ "\n"

rbra :: Int -> String
rbra n = toIndent n ++ "}" ++ "\n"

semicolon :: String
semicolon = ";" ++ "\n"

condition :: Int -> String -> String
condition n c = "if (" ++ c ++ ")" ++ "\n"

showIndented :: Int -> Statement -> String
showIndented n (Declare t v e) = toIndent n ++ show t ++ " " ++ v ++ showDef e ++ semicolon
  where
    showDef Nothing  = ""
    showDef (Just e) = " = " ++ show e
showIndented n (Assign v e) = toIndent n ++ v ++ " = " ++ show e ++ semicolon
showIndented n (If c a b) = toIndent n ++ condition n (show c) ++ showBlock (n + 1) a ++ showElse b
  where
    showElse [] = ""
    showElse b  = els n ++ showBlock (n + 1) b
    showBlock n ss = lbra n ++ concatMap (showIndented n) ss ++ rbra n
showIndented n (Assert e) = toIndent n ++ "assert " ++ show e ++ semicolon

instance Show Statement where
  show = showIndented 0

instance Show Program where
  show (Program ss) = concatMap show ss