{-# LANGUAGE FlexibleInstances #-}

module Voogie.Boogie.Pretty (
  pretty
) where

import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Voogie.Theory
import Voogie.Pretty
import Voogie.Boogie

import qualified Voogie.FOOL.Pretty as F

instance Pretty LValue where
  pretty (Variable  (Typed v _)) = v
  pretty (ArrayElem (Typed v _) e) = v ++ "[" ++ pretty e ++ "]"

instance Pretty Expression where
  pretty (IntegerLiteral i) = show i
  pretty (BooleanLiteral b) = if b then "true" else "false"

  pretty (Unary  op e)   = pretty e ++ F.pretty op
  pretty (Binary op a b) = unwords [pretty a, F.pretty op, pretty b]
  pretty (IfElse a b c)  = unwords [pretty a, "?", pretty a, ":", pretty b]

  pretty (FunApp (Typed f _) args) = f ++ "(" ++ intercalate ", " (map pretty args) ++ ")"

  pretty (Equals s a b) = unwords [pretty a, if s == Pos then "==" else "!=", pretty b]

  pretty (Ref lval) = pretty lval

indent :: Integer -> String
indent n = replicate (fromIntegral n * 2) ' '

atomic :: [String] -> String
atomic ss = unwords ss ++ ";"

braces :: Pretty s => Integer -> s -> String
braces n s = "{" ++ (if null s' then "" else "\n" ++ s' ++ indent n) ++ "}"
  where s' = indented (n + 1) s

indentedIte :: (Pretty a, Pretty b) => Integer -> Expression -> a -> b -> String
indentedIte n c a b = unwords ["if", "(" ++ pretty c ++ ")", braces n a, "else", braces n b]

instance Pretty (Typed Name) where
  pretty (Typed n t) = unwords [F.pretty t, n]

instance Pretty Statement where
  indented n s = indent n ++ case s of
    Assign pairs -> atomic [commaSep lvs, ":=", commaSep es]
      where
        (lvs, es) = NE.unzip pairs
        commaSep :: Pretty a => NonEmpty a -> String
        commaSep = intercalate ", " . fmap pretty . NE.toList
    If c False a b -> indentedIte n c a b
    If c True  a b -> indentedIte n c b a

instance Pretty [Statement] where
  indented n = unlines . map (indented n)

instance Pretty (NonEmpty Statement) where
  indented n = indented n . NE.toList