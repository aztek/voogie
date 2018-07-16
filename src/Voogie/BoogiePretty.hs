module Voogie.BoogiePretty where

import qualified Voogie.NonEmpty as VNE
import qualified Voogie.FOOL.Tuple as Tuple

import Voogie.Theory

class BoogiePretty a where
  indented :: Integer -> a -> String
  indented _ = pretty

  pretty :: a -> String
  pretty = indented 0

instance BoogiePretty Name where
  pretty n = n

instance BoogiePretty Type where
  pretty Integer = "int"
  pretty Boolean = "bool"
  pretty (Array i t) = unwords [ brackets $ VNE.intercalate ", " (fmap pretty i)
                               , pretty t ]
  pretty (TupleType ts) = parens $ Tuple.intercalate ", " (fmap pretty ts)
  pretty (Functional ts t) = unwords [ts', "->", pretty t]
    where ts' = VNE.intercalate " * " (fmap pretty ts)
  pretty (Custom n) = n

instance BoogiePretty a => BoogiePretty (Typed a) where
  pretty (Typed _ a) = pretty a

prettyTyped :: BoogiePretty a => Typed a -> String
prettyTyped (Typed t a) = unwords [pretty a ++ ":", pretty t]

instance BoogiePretty Quantifier where
  pretty Forall = "forall"
  pretty Exists = "exists"

instance BoogiePretty UnaryOp where
  pretty Negate   = "!"
  pretty Positive = "+"
  pretty Negative = "-"

instance BoogiePretty BinaryOp where
  pretty = \case
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

instance BoogiePretty Integer where
  pretty = show

instance BoogiePretty Bool where
  pretty True  = "true"
  pretty False = "false"

instance BoogiePretty Sign where
  pretty Pos = "=="
  pretty Neg = "!="

parens :: String -> String
parens s = "(" ++ s ++ ")"

brackets :: String -> String
brackets s = "[" ++ s ++ "]"
