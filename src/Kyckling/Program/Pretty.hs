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

braces :: Pretty s => Integer -> s -> String
braces n s = "{" ++ (if null s' then "" else "\n" ++ s' ++ indent n) ++ "}"
  where s' = indented (n + 1) s

indentedIte :: (Pretty a, Pretty b) => Integer -> Expression -> a -> b -> String
indentedIte n c a b = indent n ++ "if (" ++ pretty c ++ ") " ++ braces n a ++ " else " ++ braces n b

instance Pretty Statement where
  indented n s = case s of
    Declare (Typed v t) -> indent n ++ F.pretty t ++ " " ++ v ++ ";"
    Assign lv e -> indent n ++ pretty lv ++ " = " ++ pretty e ++ ";"
    If c a b -> indentedIte n c a b
    IfTerminating c False a b -> indentedIte n c a b
    IfTerminating c True  a b -> indentedIte n c b a

instance Pretty [Statement] where
  indented n = unlines . map (indented n)

instance Pretty TerminatingStatement where
  indented n (Return    ss e)     = indented n ss ++ indent n ++ "return " ++ pretty e ++ ";\n"
  indented n (IteReturn ss c a b) = indented n ss ++ indentedIte n c a b ++ "\n"

instance Pretty FunDef where
  indented n (FunDef t f vars ts) = indent n ++ prettySignature ++ braces n ts
    where
      prettySignature = F.pretty t ++ " " ++ f ++ " (" ++ prettyVars ++ ") "
      prettyVars = intercalate ", " (map (\(Typed n t) -> F.pretty t ++ " " ++ n) vars)

instance Pretty Assertion where
  pretty (Assertion f) = "assert " ++ F.pretty f ++ ";"

instance Pretty Program where
  pretty (Program fs ss as) = concatMap pretty fs ++ pretty ss ++ concatMap pretty as