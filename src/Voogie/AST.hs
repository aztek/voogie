{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Voogie.AST where

import Text.Parsec.Pos

data AST a = AST
  { position :: SourcePos
  , astValue :: a
  }
  deriving (Functor, Foldable, Traversable)

instance Show a => Show (AST a) where
  show (AST _ a) = show a

instance Eq a => Eq (AST a) where
  AST sp1 a == AST sp2 b = sp1 == sp2 && a == b
