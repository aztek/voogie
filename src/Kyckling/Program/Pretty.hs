module Kyckling.Program.Pretty (
  pretty
) where

import Data.List

import Kyckling.Theory
import Kyckling.Pretty
import Kyckling.Program

import qualified Kyckling.FOOL.Pretty as F

instance Pretty LValue where
  pretty (Variable  (Typed v _)) = v
  pretty (ArrayElem (Typed v _) e) = v ++ "[" ++ pretty e ++ "]"

instance Pretty Expression where
  pretty (IntegerConst i) = show i
  pretty (BooleanConst b) = if b then "true" else "false"

  pretty (Unary  op e)   = pretty e ++ F.pretty op
  pretty (Binary op a b) = pretty a ++ " " ++ F.pretty op ++ " " ++ pretty b
  pretty (IfElse a b c)  = pretty a ++ " ? " ++ pretty a ++ " : " ++ pretty b

  pretty (Eql   a b) = pretty a ++ " == " ++ pretty b
  pretty (InEql a b) = pretty a ++ " != " ++ pretty b

  pretty (Ref lval) = pretty lval

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
indented n (Declare (Typed v t)) = toIndent n ++ F.pretty t ++ " " ++ v ++ semicolon
indented n (Assign lv e) = toIndent n ++ pretty lv ++ " = " ++ pretty e ++ semicolon
indented n (If c a b) = toIndent n ++ condition n (pretty c) ++ prettyBlock n a ++ prettyElse b
  where
    prettyElse [] = ""
    prettyElse b  = els n ++ prettyBlock n b
    prettyBlock n ss = lbra n ++ prettyStatements (n + 1) ss ++ rbra n

instance Pretty Statement where
  pretty = indented 0

prettyStatements :: Int -> [Statement] -> String
prettyStatements n = intercalate "\n" . map (indented n)

instance Pretty Assertion where
  pretty (Assertion f) = "assert " ++ F.pretty f ++ semicolon

instance Pretty Program where
  pretty (Program ss as) = prettyStatements 0 ss ++ "\n" ++ concatMap pretty as