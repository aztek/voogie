{-# LANGUAGE FlexibleInstances #-}

module Voogie.Boogie.Pretty (
  pretty
) where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Voogie.NonEmpty as VNE
import Data.List.NonEmpty (NonEmpty)

import Voogie.Theory
import Voogie.Pretty 
import Voogie.Boogie

import Voogie.FOOL.Pretty()

instance Pretty LValue where
  pretty (LValue (Typed _ v) is) = v ++ concatMap (brackets . commaSep) is

instance Pretty Expression where
  pretty (IntegerLiteral i) = pretty i
  pretty (BooleanLiteral b) = pretty b

  pretty (Unary  op e)   = pretty e ++ pretty op
  pretty (Binary op a b) = unwords [pretty a, pretty op, pretty b]
  pretty (IfElse a b c)  = unwords [pretty a, "?", pretty b, ":", pretty c]

  pretty (FunApp (Typed _ f) args) = f ++ parens (intercalate ", " $ map pretty args)

  pretty (Equals s a b) = unwords [pretty a, pretty s, pretty b]

  pretty (Ref lval) = pretty lval

indent :: Integer -> String
indent n = replicate (fromIntegral n * 2) ' '

atomic :: [String] -> String
atomic ss = unwords ss ++ ";"

braces :: Pretty s => Integer -> s -> String
braces n s | null s'   = "{}"
           | otherwise = "{\n" ++ s' ++ indent n ++ "}"
  where s' = indented (n + 1) s

commaSep :: Pretty a => NonEmpty a -> String
commaSep = VNE.intercalate ", " . fmap pretty

indentedIte :: (Pretty a, Pretty b) => Integer -> Expression -> a -> b -> String
indentedIte n c a b = unwords ["if", parens (pretty c), braces n a, "else", braces n b]

instance Pretty Statement where
  indented n s = indent n ++ case s of
    Assign pairs -> atomic [commaSep lvs, ":=", commaSep es]
      where (lvs, es) = NE.unzip pairs
    If c False a b -> indentedIte n c a b
    If c True  a b -> indentedIte n c b a

instance Pretty [Statement] where
  indented n = unlines . map (indented n)

instance Pretty (NonEmpty Statement) where
  indented n = indented n . NE.toList
