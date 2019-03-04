module Voogie.BoogiePretty(
  module Voogie.Pretty,
  prettyTyped, boolean
) where

import Voogie.Theory
import Voogie.Pretty
import Voogie.BoogieSyntax

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

instance Pretty Type where
  pretty = \case
    Integer -> builtin typeInteger
    Boolean -> builtin typeBoolean
    Array i t -> brackets (commaSep (pretty <$> i)) <+> pretty t
    TupleType ts -> error $ "Cannot represent tuple " ++ show ts ++
                            " in the Boogie syntax."
    Functional ts t -> ts' <+> operator "->" <+> pretty t
      where ts' = sepBy (space <> operator "*" <> space) (pretty <$> ts)
    Custom n -> pretty n

prettyTyped :: Pretty a => Typed a -> Doc
prettyTyped (Typed t a) = pretty a <> punctuation opTyped <+> pretty t

instance Pretty Quantifier where
  pretty = keyword . quantifierName

instance Pretty UnaryOp where
  pretty = operator . unaryOpName

instance Pretty BinaryOp where
  pretty = operator . binaryOpName

boolean :: Bool -> Doc
boolean = builtin . booleanName

instance Pretty Sign where
  pretty = operator . signName
