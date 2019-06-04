{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE CPP #-}

module Voogie.AST where

#if !MIN_VERSION_base(4, 8, 0)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

import Text.Parsec.Pos (SourcePos)

data AST a = AST {
  position :: (SourcePos, SourcePos),
  astValue :: a
} deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (AST a) where
  show = show . astValue
