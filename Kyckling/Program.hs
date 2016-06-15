{-#LANGUAGE GADTs,DataKinds,KindSignatures,ExistentialQuantification #-}

module Kyckling.Program where

import Text.Printf
import Data.List

data Type = I | B | A Type

data LiftedType (t :: Type) where
  LiftI :: LiftedType I
  LiftB :: LiftedType B
  LiftA :: LiftedType t -> LiftedType (A t)

unliftType :: LiftedType t -> Type
unliftType LiftI = I
unliftType LiftB = B
unliftType (LiftA t) = A (unliftType t)

arrayElem :: LiftedType (A t) -> LiftedType t
arrayElem (LiftA t) = t

data Var (t :: Type) where
  Var :: LiftedType t -> String -> Var t

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
  IntegerConst :: Integer -> Expression I
  BoolConst    :: Bool -> Expression B

  Unary   :: UnaryOp   a b     -> Expression a -> Expression b
  Binary  :: BinaryOp  a b c   -> Expression a -> Expression b -> Expression c
  Ternary :: TernaryOp a b c d -> Expression a -> Expression b -> Expression c -> Expression d

  Ref :: Var t -> Expression t

typeOf :: Expression t -> LiftedType t
typeOf (IntegerConst _)  = LiftI
typeOf (BoolConst _) = LiftB
typeOf (Unary op _) = typeOf' op
  where
    typeOf' :: UnaryOp a b -> LiftedType b
    typeOf' Negate   = LiftB
    typeOf' Positive = LiftI
    typeOf' Negative = LiftI
typeOf (Binary Select arr _) = arrayElem (typeOf arr)
typeOf (Binary op _ _) = typeOf' op
  where
    typeOf' :: BinaryOp a b c -> LiftedType c
    typeOf' And      = LiftB
    typeOf' Or       = LiftB
    typeOf' Greater  = LiftB
    typeOf' Less     = LiftB
    typeOf' Geq      = LiftB
    typeOf' Leq      = LiftB
    typeOf' Add      = LiftI
    typeOf' Subtract = LiftI
    typeOf' Multiply = LiftI
    typeOf' Eq       = LiftB
    typeOf' InEq     = LiftB
typeOf (Ternary IfElse _ a _) = typeOf a
typeOf (Ref (Var t _)) = t

data LValue (t :: Type) where
  Variable  :: Var t -> LValue t
  ArrayElem :: Var (A t) -> Expression I -> LValue t

data Statement = forall t. Declare (Var t) (Maybe (Expression t))
               | forall t. Assign (LValue t) (Expression t)
               | If (Expression B) [Statement] [Statement]

data Assertion = Assertion (Expression B)

data Program = Program [Statement] [Assertion]

instance Show (LiftedType t) where
  show LiftI = "int"
  show LiftB = "bool"
  show (LiftA t) = show t ++ "[]"

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

instance Show (Expression t) where
  show (IntegerConst i) = show i
  show (BoolConst True)  = "true"
  show (BoolConst False) = "false"

  show (Unary op e) = show e ++ show op
  show (Binary Select a b) = show a ++ "[" ++ show b ++ "]"
  show (Binary op a b) = show a ++ show op ++ show b
  show (Ternary IfElse a b c) = show a ++ " ? " ++ show a ++ " : " ++ show b

  show (Ref (Var _ v)) = v

instance Show (LValue t) where
  show (Variable (Var _ v)) = v
  show (ArrayElem (Var _ v) e) = v ++ "[" ++ show e ++ "]"

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
showIndented n (Declare v e) = toIndent n ++ showVar v ++ showDef e ++ semicolon
  where
    showVar (Var t v) = show t ++ " " ++ v
    showDef Nothing  = ""
    showDef (Just e) = " = " ++ show e
showIndented n (Assign lv e) = toIndent n ++ show lv ++ " = " ++ show e ++ semicolon
showIndented n (If c a b) = toIndent n ++ condition n (show c) ++ showBlock (n + 1) a ++ showElse b
  where
    showElse [] = ""
    showElse b  = els n ++ showBlock (n + 1) b
    showBlock n ss = lbra n ++ concatMap (showIndented n) ss ++ rbra n

instance Show Assertion where
  show (Assertion e) = "assert " ++ show e ++ semicolon

instance Show Statement where
  show = showIndented 0

instance Show Program where
  show (Program ss as) = concatMap show ss ++ "\n" ++ concatMap show as