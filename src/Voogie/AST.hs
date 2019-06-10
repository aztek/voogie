{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{-|
Module       : Voogie.AST
Description  : Abstract syntax tree.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.AST (
  AST(..)
) where

import Text.Parsec.Pos (SourcePos)

data AST a = AST {
  astRange :: (SourcePos, SourcePos),
  astValue :: a
} deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (AST a) where
  show = show . astValue
