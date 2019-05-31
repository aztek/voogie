module Voogie.AST where

import Text.Parsec.Pos (SourcePos)

data AST a = AST {
  position :: (SourcePos, SourcePos),
  astValue :: a
} deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (AST a) where
  show = show . astValue
