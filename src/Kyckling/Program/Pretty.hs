{-# LANGUAGE FlexibleInstances #-}

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
  pretty (IntegerLiteral i) = show i
  pretty (BooleanLiteral b) = if b then "true" else "false"

  pretty (Unary  op e)   = pretty e ++ F.pretty op
  pretty (Binary op a b) = pretty a ++ " " ++ F.pretty op ++ " " ++ pretty b
  pretty (IfElse a b c)  = pretty a ++ " ? " ++ pretty a ++ " : " ++ pretty b

  pretty (FunApp (Typed f _) args) = f ++ "(" ++ intercalate ", " (map pretty args) ++ ")"

  pretty (Equals s a b) = pretty a ++ (if s == Pos then " == " else " != ") ++ pretty b

  pretty (Ref lval) = pretty lval

indent :: Integer -> String
indent n = replicate (fromIntegral n * 2) ' '

atomic :: [String] -> String
atomic ss = unwords ss ++ ";"

braces :: Pretty s => Integer -> s -> String
braces n s = "{" ++ (if null s' then "" else "\n" ++ s' ++ indent n) ++ "}"
  where s' = indented (n + 1) s

indentedIte :: (Pretty a, Pretty b) => Integer -> Expression -> a -> b -> String
indentedIte n c a b = "if (" ++ pretty c ++ ") " ++ braces n a ++ " else " ++ braces n b

instance Pretty (Typed Name) where
  pretty (Typed n t) = F.pretty t ++ " " ++ n

instance Pretty Statement where
  indented n s = indent n ++ case s of
    Declare v -> atomic [pretty v]
    Assign lv e -> atomic [pretty lv, "=", pretty e]
    If c a (Left b) -> indentedIte n c a b
    If c a (Right (False, b)) -> indentedIte n c a b
    If c a (Right (True,  b)) -> indentedIte n c b a

instance Pretty NonTerminating where
  indented n = unlines . map (indented n)

instance Pretty Return where
  indented n r = indent n ++ case r of
    Return e -> atomic ["return", pretty e]
    IteReturn c a b -> indentedIte n c a b

instance Pretty Terminating where
  indented n (Terminating ss r) = indented n ss ++ indented n r ++ "\n"

instance Pretty FunDef where
  indented n (FunDef f vars ts) = indent n ++ prettySignature ++ braces n ts
    where
      prettySignature = pretty f ++ "(" ++ prettyVars ++ ") "
      prettyVars = intercalate ", " (map pretty vars)

instance Pretty Assertion where
  pretty (Assertion f) = atomic ["assert", F.pretty f]

instance Pretty Program where
  pretty (Program fs ss as) = unlines (map pretty fs) ++ pretty ss ++ concatMap pretty as