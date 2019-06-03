{-# LANGUAGE LambdaCase #-}

module Voogie.TPTPretty (
  module Voogie.FOOL.TPTPretty
) where

import Voogie.FOOL.TPTPretty
import Voogie.Theory
import Voogie.TPTP
import Voogie.TPTPSyntax

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
