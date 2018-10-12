module Voogie.Pretty where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Voogie.NonEmpty as VNE

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

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

keyword = blue . text
operator = text
builtin = bold . text
number = green . integer
punctuation = text
