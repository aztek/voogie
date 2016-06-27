module Kyckling.Program.Pretty (
  prettyType,
  prettyLValue,
  prettyExpression,
  prettyProgram
) where

import Data.List

import Kyckling.Type
import Kyckling.Program

prettyType :: Type -> String
prettyType Integer = "int"
prettyType Boolean = "bool"
prettyType (Array t) = prettyType t ++ "[]"

prettyUnaryOp :: UnaryOp -> String
prettyUnaryOp Negate   = "!"
prettyUnaryOp Positive = "+"
prettyUnaryOp Negative = "-"

prettyBinaryOp :: BinaryOp -> String
prettyBinaryOp op =
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

prettyLValue :: LValue -> String
prettyLValue (Variable (Var v _)) = v
prettyLValue (ArrayElem (Var v _) e) = v ++ "[" ++ prettyExpression e ++ "]"

prettyExpression :: Expression -> String
prettyExpression (IntegerConst i)  = show i
prettyExpression (BoolConst True)  = "true"
prettyExpression (BoolConst False) = "false"

prettyExpression (Unary op e)    = prettyExpression e ++ prettyUnaryOp op
prettyExpression (Binary op a b) = prettyExpression a ++ " " ++ prettyBinaryOp op ++ " " ++ prettyExpression b
prettyExpression (IfElse a b c)  = prettyExpression a ++ " ? " ++ prettyExpression a ++ " : " ++ prettyExpression b

prettyExpression (Eql   a b) = prettyExpression a ++ " == " ++ prettyExpression b
prettyExpression (InEql a b) = prettyExpression a ++ " != " ++ prettyExpression b

prettyExpression (Ref lval) = prettyLValue lval

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

indented :: Int -> Statement -> String
indented n (Declare (Var v t)) = toIndent n ++ prettyType t ++ " " ++ v ++ semicolon
indented n (Assign lv e) = toIndent n ++ prettyLValue lv ++ " = " ++ prettyExpression e ++ semicolon
indented n (If c a b) = toIndent n ++ condition n (prettyExpression c) ++ prettyBlock n a ++ prettyElse b
  where
    prettyElse [] = ""
    prettyElse b  = els n ++ prettyBlock n b
    prettyBlock n ss = lbra n ++ prettyStatements (n + 1) ss ++ rbra n

prettyStatement :: Statement -> String
prettyStatement = indented 0

prettyStatements :: Int -> [Statement] -> String
prettyStatements n = intercalate "\n" . map (indented n)

prettyAssertion :: Assertion -> String
prettyAssertion (Assertion e) = "assert " ++ prettyExpression e ++ semicolon

prettyProgram :: Program -> String
prettyProgram (Program ss as) = prettyStatements 0 ss ++ "\n" ++ concatMap prettyAssertion as