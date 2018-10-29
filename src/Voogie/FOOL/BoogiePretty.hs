module Voogie.FOOL.BoogiePretty (pretty) where

import Voogie.Theory
import Voogie.FOOL
import Voogie.BoogieSyntax
import Voogie.BoogiePretty

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

instance Pretty a => Pretty (Typed a) where
  pretty (Typed _ a) = pretty a

instance Pretty Var where
  pretty (Var v) = text v

pretty' :: Term -> Doc
pretty' t@Binary{} = parens (pretty t)
pretty' t@Equals{} = parens (pretty t)
pretty' t@If{} = parens (pretty t)
pretty' t = pretty t

instance Pretty Term where
  pretty = \case
    IntegerConstant i -> number i
    BooleanConstant b -> boolean b
    Variable v -> pretty v
    Constant f -> pretty f
    Application f as -> funapp (pretty f) (pretty <$> as)
    Binary op a b -> pretty'' a <+> pretty op <+> pretty'' b
      where
        pretty'' t@(Binary op' _ _) = case op `comparePrecedence` op' of
          LT -> pretty t
          EQ | op == op' && isAssociative op -> pretty t
          _ -> parens (pretty t)
        pretty'' t@Equals{} = case comparePrecedenceEquality op of
          GT -> pretty t
          _ -> parens (pretty t)
        pretty'' t = pretty' t
    Unary op t -> pretty op <> pretty' t
    Quantify q vs t -> parens $ pretty q <+> commaSep (prettyTyped <$> vs)
                            <+> punctuation "::" <+> pretty t
    Equals s a b -> pretty'' a <+> pretty s <+> pretty'' b
      where
        pretty'' t@(Binary op _ _) = case comparePrecedenceEquality op of
          LT -> pretty t
          _ -> parens (pretty t)
        pretty'' t = pretty' t
    If c a b -> pretty c <+> punctuation "?" <+> pretty a
                         <+> punctuation ":" <+> pretty b
    Select a i -> pretty a <> brackets (pretty i)
    Store a i v -> pretty a <> brackets (pretty i) <+> operator opAssign <+> pretty v
    t@Let{} -> error $ "Cannot represent let-expression " ++ show t ++
                       " in the Boogie syntax."
    t@TupleLiteral{} -> error $ "Cannot represent tuple literal " ++ show t ++
                                " in the Boogie syntax."
