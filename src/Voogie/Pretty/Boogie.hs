{-# LANGUAGE LambdaCase #-}

{-|
Module       : Voogie.Pretty.Boogie
Description  : Helper functions for the pretty printer in the Boogie syntax.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.Pretty.Boogie (
  module Voogie.Pretty,
  module Voogie.Boogie.Syntax,
  prettyTyped,
  boolean
) where

import Voogie.Boogie.Syntax
import Voogie.Pretty
import Voogie.Language

instance Pretty Type where
  pretty = \case
    Integer         -> builtin typeInteger
    Boolean         -> builtin typeBoolean
    Array       i t -> brackets (commaSep (pretty <$> i)) <+> pretty t
    Tuple        ts -> error $ "Cannot represent tuple " ++ show ts ++ " in the Boogie syntax."
    Functional ts t -> sepBy (space <> operator "*" <> space) (pretty <$> ts) 
                   <+> operator "->" <+> pretty t
    Custom        n -> pretty n

prettyTyped :: Pretty a => Typed a -> Doc
prettyTyped (Typed t a) = pretty a <> punctuation opTyped <+> pretty t

instance Pretty Quantifier where
  pretty = keyword . nameOf

instance Pretty UnaryOp where
  pretty = operator . nameOf

instance Pretty BinaryOp where
  pretty = operator . nameOf

boolean :: Bool -> Doc
boolean = builtin . nameOf

instance Pretty Sign where
  pretty = operator . nameOf
