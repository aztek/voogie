module Voogie.TPTP where

import Voogie.Theory
import Voogie.FOOL

data Unit
  = TypeDeclaration Name Name
  | SymbolDeclaration Name Identifier
  | Axiom Name Formula
  | Conjecture Name Formula
  deriving (Show, Eq)

unitName :: Unit -> Name
unitName = \case
  TypeDeclaration n _ -> n
  SymbolDeclaration n _ -> n
  Axiom n _ -> n
  Conjecture n _ -> n

data InputType
  = TypeInputType
  | AxiomInputType
  | ConjectureInputType
  deriving (Show, Eq, Ord, Bounded, Enum)

unitInputType :: Unit -> InputType
unitInputType = \case
  TypeDeclaration{} -> TypeInputType
  SymbolDeclaration{} -> TypeInputType
  Axiom{} -> AxiomInputType
  Conjecture{} -> ConjectureInputType

newtype TPTP = TPTP [Unit]
  deriving (Show, Eq)

toTPTP :: Problem -> TPTP
toTPTP (Problem types symbols axioms conjecture) =
  TPTP (tptpTypes ++ tptpSymbols ++ tptpAxioms ++ [tptpConjecture conjecture])
  where
    tptpTypes = tptpType <$> types
    tptpSymbols = tptpSymbol <$> symbols
    tptpAxioms = tptpAxiom <$> zip [(1::Integer)..] axioms
    
    tptpType t = TypeDeclaration t t
    tptpSymbol n@(Typed _ s) = SymbolDeclaration s n
    tptpAxiom (nr, f) = Axiom ("voogie_precondition_" ++ show nr) f
    tptpConjecture = Conjecture "voogie_conjecture"
