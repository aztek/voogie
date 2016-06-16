module Kyckling.Pretty (
  showType,
  showLValue,
  showExpression,
  showProgram
) where

import Data.List

import Kyckling.Program

showType :: Type -> String
showType Integer = "int"
showType Boolean = "bool"
showType (Array t) = showType t ++ "[]"

showUnaryOp :: UnaryOp -> String
showUnaryOp Negate   = "!"
showUnaryOp Positive = "+"
showUnaryOp Negative = "-"

showBinaryOp :: BinaryOp -> String
showBinaryOp op =
  case op of
    And      -> "&&"
    Or       -> "||"
    Greater  -> ">"
    Less     -> "<"
    Geq      -> ">="
    Leq      -> "<="
    Add      -> "+"
    Subtract -> "-"
    Multiply -> "*"

showLValue :: LValue -> String
showLValue (Variable (Var v _)) = v
showLValue (ArrayElem (Var v _) e) = v ++ "[" ++ showExpression e ++ "]"

showExpression :: Expression -> String
showExpression (IntegerConst i)  = show i
showExpression (BoolConst True)  = "true"
showExpression (BoolConst False) = "false"

showExpression (Unary op e) = showExpression e ++ showUnaryOp op
showExpression (Binary op a b) = showExpression a ++ " " ++ showBinaryOp op ++ " " ++ showExpression b
showExpression (Ternary IfElse a b c) = showExpression a ++ " ? " ++ showExpression a ++ " : " ++ showExpression b

showExpression (Eql   a b) = showExpression a ++ " == " ++ showExpression b
showExpression (InEql a b) = showExpression a ++ " != " ++ showExpression b

showExpression (Ref lval) = showLValue lval

toIndent :: Int -> String
toIndent n = replicate (n * 2) ' '

els :: Int -> String
els n = "else "

lbra :: Int -> String
lbra n = "{" ++ "\n"

rbra :: Int -> String
rbra n = "\n" ++ toIndent n ++ "} "

semicolon :: String
semicolon = ";"

condition :: Int -> String -> String
condition n c = "if (" ++ c ++ ") "

showIndented :: Int -> Statement -> String
showIndented n (Declare v e) = toIndent n ++ showVar v ++ showDef e ++ semicolon
  where
    showVar (Var v t) = showType t ++ " " ++ v
    showDef Nothing  = ""
    showDef (Just e) = " = " ++ showExpression e
showIndented n (Assign lv e) = toIndent n ++ showLValue lv ++ " = " ++ showExpression e ++ semicolon
showIndented n (If c a b) = toIndent n ++ condition n (showExpression c) ++ showBlock n a ++ showElse b
  where
    showElse [] = ""
    showElse b  = els n ++ showBlock n b
    showBlock n ss = lbra n ++ showStatements (n + 1) ss ++ rbra n

showStatement :: Statement -> String
showStatement = showIndented 0

showStatements :: Int -> [Statement] -> String
showStatements n = intercalate "\n" . map (showIndented n)

showAssertion :: Assertion -> String
showAssertion (Assertion e) = "assert " ++ showExpression e ++ semicolon

showProgram :: Program -> String
showProgram (Program ss as) = showStatements 0 ss ++ "\n" ++ concatMap showAssertion as