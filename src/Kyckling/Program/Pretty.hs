{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

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

indent :: Integer -> String
indent n = replicate (fromIntegral n * 2) ' '

instance Pretty Statement where
  indented n s = indent n ++ case s of
    Declare (Typed v t) -> F.pretty t ++ " " ++ v ++ ";"
    Assign lv e -> pretty lv ++ " = " ++ pretty e ++ ";"
    If c a b -> "if (" ++ pretty c ++ ") " ++
                  indented n a ++
                  if null b then "" else "else " ++ indented n b

instance Pretty [Statement] where
  indented n ss = "{\n" ++ intercalate "\n" (map (indented $ n + 1) ss) ++ "\n" ++ indent n ++ "} "
  pretty = intercalate "\n" . map pretty

instance Pretty FunDef where
  pretty = undefined

instance Pretty Assertion where
  pretty (Assertion f) = "assert " ++ F.pretty f ++ ";"

instance Pretty Program where
  pretty (Program fs ss as) = concatMap pretty fs ++ "\n" ++ pretty ss ++ "\n" ++ concatMap pretty as