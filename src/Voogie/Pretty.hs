module Voogie.Pretty (
  module Text.PrettyPrint.ANSI.Leijen,
  sepBy,
  commaSep,
  tuple,
  funapp,
  funapp1,
  funapp2,
  funapp3,
  keyword,
  operator,
  builtin,
  punctuation,
  number
) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Voogie.NonEmpty as VNE

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Voogie.Theory (Name)

sepBy :: Doc -> NonEmpty Doc -> Doc
sepBy s (d :| ds) = d <> mconcat (fmap (s <>) ds)

commaSep :: NonEmpty Doc -> Doc
commaSep = sepBy (comma <> space)

tuple :: NonEmpty Doc -> Doc
tuple = brackets . align . commaSep

funapp :: Doc -> NonEmpty Doc -> Doc
funapp f as = f <> parens (align (commaSep as))

funapp1 :: Doc -> Doc -> Doc
funapp1 f a = funapp f (VNE.one a)

funapp2 :: Doc -> Doc -> Doc -> Doc
funapp2 f a b = funapp f (VNE.two a b)

funapp3 :: Doc -> Doc -> Doc -> Doc -> Doc
funapp3 f a b c = funapp f (VNE.three a b c)

keyword, operator, builtin, punctuation :: Name -> Doc
keyword = blue . text
operator = text
builtin = bold . text
punctuation = text

number :: Integer -> Doc
number = green . integer
