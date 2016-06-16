{-#LANGUAGE GADTs,DataKinds,KindSignatures,RankNTypes #-}

module Kyckling.Program {-(
  T(I, B, A), TType(TType),
  Type (LiftI, LiftB, LiftA), unliftType,
  Var (Var),
  UnaryOp (Negate, Positive, Negative),
  TypedUnaryOp (TypedUnaryOp),
  --BinaryOp(...),
  --TypedBinaryOp(...),
  LValue (Variable, ArrayElem),
  Expression (IntegerConst, BoolConst, Unary, Binary, Ternary, Ref),
  TypedExpression (TypedExpression),
  Assertion (Assertion),
  Program (Program)
  )-} where

import Text.Printf
import Data.List

data T = I | B | A T deriving (Eq)

data Type (t :: T) where
  LiftI :: Type I
  LiftB :: Type B
  LiftA :: Type t -> Type (A t)

unliftType :: Type t -> T
unliftType LiftI = I
unliftType LiftB = B
unliftType (LiftA t) = A (unliftType t)

--instance Eq (Type t) where
--  a == b = unliftType a == unliftType b

data TType = TType (forall t . Type t)
instance Eq TType where
  TType a == TType b = unliftType a == unliftType b

liftType :: T -> TType
liftType = undefined
--liftType I = TType LiftI
--liftType B = TType LiftB
--liftType (A t) = TType $ LiftA t'
--  where liftType t

data Var (t :: T) where
  Var :: Type t -> String -> Var t

data UnaryOp (a :: T) (b :: T) where
  Negate   :: UnaryOp B B
  Positive :: UnaryOp I I
  Negative :: UnaryOp I I

data TypedUnaryOp = forall a b . TypedUnaryOp (Type a) (Type b) (UnaryOp a b)

--data TypedUnaryOp where
  --TypedUnaryOp :: { range :: Type a, domain :: Type b, op :: UnaryOp a b } -> TypedUnaryOp

data BinaryOp (a :: T) (b :: T) (c :: T) where
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

data TypedBinaryOp = forall a b c . TypedBinaryOp (Type a) (Type b) (Type c) (BinaryOp a b c)

data TernaryOp (a :: T) (b :: T) (c :: T) (d :: T) where
  IfElse :: TernaryOp B a a a

data TypedTernaryOp = forall a b c d . TypedTernaryOp (Type a) (Type b) (Type c) (Type d) (TernaryOp a b c d)

data LValue (t :: T) where
  Variable  :: Var t -> LValue t
  ArrayElem :: Var (A t) -> Expression I -> LValue t

data TypedLValue = forall t . TypedLValue (Type t) (LValue t)

data Expression (t :: T) where
  IntegerConst :: Integer -> Expression I
  BoolConst    :: Bool -> Expression B

  Unary   :: UnaryOp   a b     -> Expression a -> Expression b
  Binary  :: BinaryOp  a b c   -> Expression a -> Expression b -> Expression c
  Ternary :: TernaryOp a b c d -> Expression a -> Expression b -> Expression c -> Expression d

  Ref :: LValue t -> Expression t

--data TypedExpression where
  --TypedExpression :: { typeOf :: Type t, expr :: Expression t } -> TypedExpression

data TypedExpression = forall t . TypedExpression (Type t) (Expression t)

--expr :: TypedExpression -> Expression t
--expr 

--typeOf :: TypedExpression -> Type t
--typeOf (TypedExpression t _) = t
{-
typeOf :: Expression t -> Type t
typeOf (IntegerConst _)  = LiftI
typeOf (BoolConst _) = LiftB
typeOf (Unary op _) = typeOf' op
  where
    typeOf' :: UnaryOp a b -> Type b
    typeOf' Negate   = LiftB
    typeOf' Positive = LiftI
    typeOf' Negative = LiftI
typeOf (Binary Select arr _) = arrayElem (typeOf arr)
  where
    arrayElem :: Type (A t) -> Type t
    arrayElem (LiftA t) = t
typeOf (Binary op _ _) = typeOf' op
  where
    typeOf' :: BinaryOp a b c -> Type c
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
typeOf (Ref _) = undefined -- TODO
-}
data Statement = forall t . Declare (Var t) (Maybe (Expression t))
               | forall t . Assign (LValue t) (Expression t)
               | If (Expression B) [Statement] [Statement]

data Assertion = Assertion (Expression B)

data Program = Program [Statement] [Assertion]

instance Show (Type t) where
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

instance Show (LValue t) where
  show (Variable (Var _ v)) = v
  show (ArrayElem (Var _ v) e) = v ++ "[" ++ show e ++ "]"

instance Show (Expression t) where
  show (IntegerConst i) = show i
  show (BoolConst True)  = "true"
  show (BoolConst False) = "false"

  show (Unary op e) = show e ++ show op
  show (Binary Select a b) = show a ++ "[" ++ show b ++ "]"
  show (Binary op a b) = show a ++ show op ++ show b
  show (Ternary IfElse a b c) = show a ++ " ? " ++ show a ++ " : " ++ show b

  show (Ref lval) = show lval

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