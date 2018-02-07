{-# LANGUAGE FlexibleInstances #-}

module Voogie.Pretty where

import qualified Voogie.NonEmpty as VNE
import qualified Voogie.FOOL.Tuple as Tuple

import Voogie.Theory

class Pretty a where
  indented :: Integer -> a -> String
  indented _ = pretty

  pretty :: a -> String
  pretty = indented 0

instance Pretty Name where
  pretty n = n

instance Pretty Type where
  pretty Integer = "int"
  pretty Boolean = "bool"
  pretty (Array i t) = unwords [ brackets $ VNE.intercalate ", " (fmap pretty i)
                               , pretty t ]
  pretty (TupleType ts) = parens $ Tuple.intercalate ", " (fmap pretty ts)
  pretty (Functional ts t) = unwords [ts', "->", pretty t]
    where ts' = VNE.intercalate " * " (fmap pretty ts)
  pretty (Custom n) = n

instance Pretty a => Pretty (Typed a) where
  pretty (Typed _ a) = pretty a

prettyTyped :: Pretty a => Typed a -> String
prettyTyped (Typed t a) = unwords [pretty a ++ ":", pretty t]

instance Pretty Quantifier where
  pretty Forall = "forall"
  pretty Exists = "exists"

instance Pretty UnaryOp where
  pretty Negate   = "!"
  pretty Positive = "+"
  pretty Negative = "-"

instance Pretty BinaryOp where
  pretty op = case op of
    And      -> "&&"
    Or       -> "||"
    Imply    -> "=>"
    Iff      -> "=="
    Xor      -> "!="
    Greater  -> ">"
    Less     -> "<"
    Geq      -> ">="
    Leq      -> "<="
    Add      -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide   -> "/"

instance Pretty Integer where
  pretty = show

instance Pretty Bool where
  pretty True  = "true"
  pretty False = "false"

instance Pretty Sign where
  pretty Pos = "=="
  pretty Neg = "!="

parens :: String -> String
parens s = "(" ++ s ++ ")"

brackets :: String -> String
brackets s = "[" ++ s ++ "]"
