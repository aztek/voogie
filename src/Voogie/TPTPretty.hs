module Voogie.TPTPretty() where

import Voogie.Theory
import Voogie.FOOL
import Voogie.TPTP

import Voogie.Pretty
import Voogie.FOOL.TPTPretty()

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

tff :: String -> String -> Doc -> Doc
tff n it s = funapp3 (keyword "tff") (text n) (keyword it) s <> punctuation "."

prettyTypeDeclaration :: Name -> Doc
prettyTypeDeclaration n = tff n "type" (text n <> colon <+> keyword "$tType")

prettySymbolDeclaration :: Typed Name -> Doc
prettySymbolDeclaration n@(Typed _ s) = tff s "type" (pretty n)

prettyAxiom :: (Integer, Formula) -> Doc
prettyAxiom (nr, f) = tff ("voogie_precondition_" ++ show nr)
                          "axiom" (line <> pretty f) <> line

prettyConjecture :: Formula -> Doc
prettyConjecture f = tff "voogie_conjecture" "conjecture" (line <> pretty f)

instance Pretty TPTP where
  pretty (TPTP types symbols axioms conjecture) =
       vsep (prettyTypes ++ prettySymbols)
    <> line
    <> line
    <> vsep prettyAxioms
    <> line
    <> prettyConjecture conjecture
    where
      prettyTypes   = prettyTypeDeclaration <$> types
      prettySymbols = prettySymbolDeclaration <$> symbols
      prettyAxioms  = prettyAxiom <$> zip [1..] axioms
