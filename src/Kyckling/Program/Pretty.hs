module Kyckling.Program.Pretty (
  F.prettyType,
  prettyLValue,
  prettyExpression,
  prettyProgram
) where

import Data.List

import Kyckling.Theory
import Kyckling.Program

import qualified Kyckling.FOOL.Pretty as F

prettyLValue :: LValue -> String
prettyLValue (Variable  (Typed v _)) = v
prettyLValue (ArrayElem (Typed v _) e) = v ++ "[" ++ prettyExpression e ++ "]"

prettyExpression :: Expression -> String
prettyExpression (IntegerConst i) = show i
prettyExpression (BooleanConst b) = if b then "true" else "false"

prettyExpression (Unary  op e)   = prettyExpression e ++ F.prettyUnaryOp op
prettyExpression (Binary op a b) = prettyExpression a ++ " " ++ F.prettyBinaryOp op ++ " " ++ prettyExpression b
prettyExpression (IfElse a b c)  = prettyExpression a ++ " ? " ++ prettyExpression a ++ " : " ++ prettyExpression b

prettyExpression (Eql   a b) = prettyExpression a ++ " == " ++ prettyExpression b
prettyExpression (InEql a b) = prettyExpression a ++ " != " ++ prettyExpression b

prettyExpression (Ref lval) = prettyLValue lval

toIndent :: Int -> String
toIndent n = replicate (n * 2) ' '

els :: Int -> String
els n = "else "

lbra :: Int -> String
lbra n = "{\n"

rbra :: Int -> String
rbra n = "\n" ++ toIndent n ++ "} "

semicolon :: String
semicolon = ";"

condition :: Int -> String -> String
condition n c = "if (" ++ c ++ ") "

indented :: Int -> Statement -> String
indented n (Declare (Typed v t)) = toIndent n ++ F.prettyType t ++ " " ++ v ++ semicolon
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
prettyAssertion (Assertion f) = "assert " ++ F.prettyFormula f ++ semicolon

prettyProgram :: Program -> String
prettyProgram (Program ss as) = prettyStatements 0 ss ++ "\n" ++ concatMap prettyAssertion as