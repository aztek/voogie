{-# LANGUAGE FlexibleInstances #-}

module Voogie.Boogie.Pretty (pretty) where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Voogie.NonEmpty as VNE
import Data.List.NonEmpty (NonEmpty)

import Voogie.Pretty
import Voogie.Boogie

import Voogie.FOOL.Pretty()

instance Pretty LValue where
  pretty (LValue v is) = pretty v ++ concatMap (brackets . commaSep) is

instance Pretty Expression where
  pretty e = case e of
    IntegerLiteral i -> pretty i
    BooleanLiteral b -> pretty b
    Unary  op e -> pretty op ++ parens (pretty e)
    Binary op a b -> unwords [pretty a, pretty op, pretty b]
    IfElse a b c -> unwords [pretty a, "?", pretty b, ":", pretty c]
    FunApp f as -> pretty f ++ parens (intercalate ", " $ map pretty as)
    Equals s a b -> unwords [pretty a, pretty s, pretty b]
    Ref lv -> pretty lv

indent :: Integer -> String
indent n = replicate (fromIntegral n * 2) ' '

atomic :: Integer -> [String] -> String
atomic n ss = indent n ++ unwords ss ++ ";\n"

braces :: Pretty s => Integer -> s -> String
braces n s | null s'   = "{}"
           | otherwise = "{\n" ++ s' ++ indent n ++ "}"
  where s' = indented (n + 1) s

commaSep :: Pretty a => NonEmpty a -> String
commaSep = VNE.intercalate ", " . fmap pretty

indentedIte :: Integer -> Expression -> [Statement] -> [Statement] -> String
indentedIte n c a b = indent n ++ unwords (thenBranch ++ elseBranch) ++ "\n"
  where
    thenBranch = ["if", parens (pretty c), braces n a]
    elseBranch = if null b then [] else ["else", braces n b]

instance Pretty Statement where
  indented n s = case s of
    Assign pairs -> atomic n [commaSep lvs, ":=", commaSep es]
      where (lvs, es) = NE.unzip pairs
    If c False a b -> indentedIte n c (NE.toList a) b
    If c True  a b -> indentedIte n c b (NE.toList a)

instance Pretty Assume where
  indented n (Assume f) = atomic n ["assume", pretty f]

instance Pretty TopLevel where
  indented n (Left stmt) = indented n stmt
  indented n (Right ass) = indented n ass

instance Pretty [Statement] where
  indented n = concatMap (indented n)

instance Pretty [TopLevel] where
  indented n = concatMap (indented n)

instance Pretty Main where
  pretty (Main pre stmts post) =
       "procedure main()\n"
    ++ concatMap prettyPre pre
    ++ concatMap prettyPost post
    ++ braces 0 stmts
    where
      prettyPre f = atomic 1 ["requires", pretty f]
      prettyPost f = atomic 1 ["ensures", pretty f]

instance Pretty Boogie where
  pretty (Boogie vars main) = prettyVars ++ pretty main
    where
      prettyVars = concatMap prettyVarDecl vars
      prettyVarDecl t = atomic 0 ["var", prettyTyped t]
