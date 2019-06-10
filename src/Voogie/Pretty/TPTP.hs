{-# LANGUAGE LambdaCase #-}

{-|
Module       : Voogie.Pretty.TPTP
Description  : Helper functions for the pretty printer in the TPTP syntax.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.Pretty.TPTP (
  module Voogie.Pretty.TPTP.FOOL
) where

import Voogie.Pretty.TPTP.FOOL
import Voogie.Language
import Voogie.TPTP
import Voogie.TPTP.Syntax

instance Pretty InputType where
  pretty = keyword . \case
    TypeInputType       -> kwdType
    AxiomInputType      -> kwdAxiom
    ConjectureInputType -> kwdConjecture

instance Pretty Unit where
  pretty unit = funapp3 (keyword kwdTtf) (text name) (pretty it) contents
             <> punctuation "."
    where
      name = nameOf unit
      it = unitInputType unit
      contents = case unit of
        TypeDeclaration   _ n -> text n <> colon <+> keyword kwdTypeDecl
        SymbolDeclaration _ i -> pretty i
        Axiom             _ f -> line <> pretty f
        Conjecture        _ f -> line <> pretty f

instance Pretty TPTP where
  pretty (TPTP units) = vsep (pretty <$> units) <> line
