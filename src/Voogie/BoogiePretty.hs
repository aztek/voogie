module Voogie.BoogiePretty where

import Data.List.NonEmpty (NonEmpty(..))

import Voogie.Theory

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

keyword = text
operator = text
builtin = text
number = integer
punctuation = text

instance Pretty Type where
  pretty Integer = builtin "int"
  pretty Boolean = builtin "bool"
  pretty (Array i t) = brackets (commaSep (pretty <$> i)) <+> pretty t
  pretty (TupleType ts) = error $ "Cannot represent tuple " ++ show ts ++
                                  " in the Boogie syntax."
  pretty (Functional ts t) = ts' <+> operator "->" <+> pretty t
    where ts' = sepBy (space <> operator "*" <> space) (pretty <$> ts)
  pretty (Custom n) = pretty n

instance Pretty a => Pretty (Typed a) where
  pretty (Typed _ a) = pretty a

prettyTyped :: Pretty a => Typed a -> Doc
prettyTyped (Typed t a) = pretty a <> punctuation ":" <+> pretty t

instance Pretty Quantifier where
  pretty = keyword . \case
    Forall -> "forall"
    Exists -> "exists"

instance Pretty UnaryOp where
  pretty = operator . \case
    Negate   -> "!"
    Positive -> "+"
    Negative -> "-"

instance Pretty BinaryOp where
  pretty = operator . \case
    And      -> "&&"
    Or       -> "||"
    Imply    -> "==>"
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

boolean :: Bool -> Doc
boolean = builtin . \case
  True  -> "true"
  False -> "false"

instance Pretty Sign where
  pretty = operator . \case
    Pos -> "=="
    Neg -> "!="

sepBy :: Doc -> NonEmpty Doc -> Doc
sepBy s (d :| ds) = d <> mconcat (fmap (s <>) ds)

commaSep :: NonEmpty Doc -> Doc
commaSep = sepBy (comma <> space)
