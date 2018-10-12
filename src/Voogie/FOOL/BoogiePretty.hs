module Voogie.FOOL.BoogiePretty (pretty) where

import Voogie.BoogiePretty
import Voogie.FOOL

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

instance Pretty Var where
  pretty (Var v) = text v

instance Pretty Term where
  pretty = \case
    IntegerConstant i -> number i
    BooleanConstant b -> boolean b
    Variable v -> pretty v
    Constant f -> pretty f
    Application f as -> funapp (pretty f) (pretty <$> as)
    Binary op a b -> parens $ pretty a <+> pretty op <+> pretty b
    Unary op t -> pretty op <> pretty t
    Quantify q vs t -> parens $ pretty q <+> commaSep (prettyTyped <$> vs)
                            <+> punctuation "::" <+> pretty t
    Equals s a b -> parens $ pretty a <+> pretty s <+> pretty b
    If c a b -> pretty c <+> punctuation "?" <+> pretty a
                         <+> punctuation ":" <+> pretty b
    Select a i -> pretty a <> brackets (pretty i)
    Store a i v -> pretty a <> brackets (pretty i) <+> operator ":=" <+> pretty v
    t@Let{} -> error $ "Cannot represent let-expression " ++ show t ++
                       " in the Boogie syntax."
    t@TupleLiteral{} -> error $ "Cannot represent tuple literal " ++ show t ++
                                " in the Boogie syntax."
