{-# LANGUAGE LambdaCase #-}

module Voogie.TPTP (
  Unit(..),
  InputType(..),
  unitInputType,
  TPTP(..),
  toTPTP
) where

import Voogie.FOOL

data Unit
  = TypeDeclaration Name Name
  | SymbolDeclaration Name Identifier
  | Axiom Name Formula
  | Conjecture Name Formula
  deriving (Show, Eq)

instance Named Unit where
  nameOf = \case
    TypeDeclaration   n _ -> n
    SymbolDeclaration n _ -> n
    Axiom             n _ -> n
    Conjecture        n _ -> n

data InputType
  = TypeInputType
  | AxiomInputType
  | ConjectureInputType
  deriving (Show, Eq, Ord, Bounded, Enum)

unitInputType :: Unit -> InputType
unitInputType = \case
  TypeDeclaration{}   -> TypeInputType
  SymbolDeclaration{} -> TypeInputType
  Axiom{}             -> AxiomInputType
  Conjecture{}        -> ConjectureInputType

newtype TPTP = TPTP [Unit]
  deriving (Show, Eq)

toTPTP :: Problem -> TPTP
toTPTP (Problem ts ss as c) = TPTP units
  where
    units = tptpTypes ++ tptpSymbols ++ tptpAxioms ++ [tptpConjecture c]

    tptpTypes   = tptpType   <$> ts
    tptpSymbols = tptpSymbol <$> ss
    tptpAxioms  = tptpAxiom  <$> zip [(1::Integer)..] as

    tptpType t = TypeDeclaration t t
    tptpSymbol n = SymbolDeclaration (valueOf n) n
    tptpAxiom (nr, f) = Axiom ("voogie_precondition_" ++ show nr) f
    tptpConjecture = Conjecture "voogie_conjecture"
