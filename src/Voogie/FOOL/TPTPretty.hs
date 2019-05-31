module Voogie.FOOL.TPTPretty() where

import Data.Char (toUpper, toLower)
import qualified Data.List.NonEmpty as NE (nonEmpty)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Voogie.FOOL.Tuple as Tuple
import Voogie.FOOL
import Voogie.Theory
import Voogie.Pretty
import Voogie.TPTPSyntax

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

instance Pretty BinaryOp where
  pretty op = if isInfix op then operator op' else builtin op'
    where op' = nameOf op

instance Pretty UnaryOp where
  pretty op = if isPrefix op then operator op' else builtin op'
    where op' = nameOf op

instance Pretty Type where
  pretty = \case
    Boolean -> builtin boolName
    Integer -> builtin intName
    Array i t -> foldr (funapp2 $ builtin arrayName) (pretty t) (pretty <$> i)
    TupleType ts -> tuple (Tuple.toNonEmpty (pretty <$> ts))
    Functional ts t -> ts' <+> operator ">" <+> pretty t
      where ts' = sepBy (space <> operator "*" <> space) (pretty <$> ts)
    Custom n -> text n

instance Pretty Var where
  pretty (Var n)
    | Just (c :| cs) <- NE.nonEmpty n = text (toUpper c : cs)
    | otherwise = error "empty variable name"

instance Pretty a => Pretty (Typed a) where
  pretty (Typed t s) = pretty s <> punctuation ":" <+> pretty t

prettyIdentifier :: Identifier -> Doc
prettyIdentifier i
  | Just (c :| cs) <- NE.nonEmpty (valueOf i) = text (toLower c : cs)
  | otherwise = error "empty identifier"

instance Pretty Definition where
  pretty = \case
    ConstantSymbol c -> prettyIdentifier c
    Function c vs -> funapp (prettyIdentifier c) (pretty <$> vs)
    TupleD es -> tuple (Tuple.toNonEmpty (prettyIdentifier <$> es))

instance Pretty Binding where
  pretty (Binding d t) = pretty d <+> operator opAssign <+> pretty t

instance Pretty Quantifier where
  pretty = operator . nameOf

instance Pretty Sign where
  pretty = operator . nameOf

pretty' :: Term -> Doc
pretty' t = case t of
  Binary op _ _ | isInfix op -> parens (pretty t)
  Unary op _ | isPrefix op -> parens (pretty t)
  Equals{} -> parens (pretty t)
  Quantify{} -> parens (pretty t)
  _ -> pretty t

instance Pretty Term where
  pretty = \case
    IntegerConstant n -> number n
    BooleanConstant b -> builtin (nameOf b)

    Variable (Typed _ v) -> pretty v
    Constant f -> prettyIdentifier f
    Application f as -> funapp (prettyIdentifier f) (pretty <$> as)

    Unary op a | isPrefix op -> pretty op <> pretty' a
               | otherwise   -> funapp1 (pretty op) (pretty' a)

    Binary op a b | isInfix op -> pretty'' a <+> pretty op <+> pretty'' b
                  | otherwise  -> funapp2 (pretty op) (pretty' a) (pretty' b)
      where
        pretty'' t = case t of
          Binary op' _ _ | op == op' && isAssociative op -> pretty t
          _ -> pretty' t

    Quantify q vs t -> pretty q <> tuple (pretty <$> vs)
                                <> punctuation ":" <+> pretty'' t
      where
        pretty'' t = case t of
          Binary op _ _ | not (isInfix op) -> pretty t
          Unary op _ | not (isPrefix op) -> pretty t
          _ -> pretty' t

    Equals s a b -> pretty' a <+> pretty s <+> pretty' b

    Let b@(Binding d _) t -> builtin kwdLet <> parens args
      where
        args = case t of
          Let{} -> align definition <> line <> pretty t
          _     -> align (definition <> line <> pretty t)
        definition = typeSignature <> comma <> line <> pretty b <> comma
        typeSignature = case d of
          ConstantSymbol c -> pretty c
          Function c _ -> pretty c
          TupleD es -> tuple (Tuple.toNonEmpty (pretty <$> es))

    If c a b -> funapp3 (builtin kwdIf)
                        (pretty c)
                        (line <> pretty a)
                        (line <> pretty b)

    Select a i   -> funapp2 (builtin kwdSelect) (pretty a) (pretty i)
    Store  a i e -> funapp3 (builtin kwdStore)  (pretty a) (pretty i) (pretty e)

    TupleLiteral ts -> tuple (Tuple.toNonEmpty (pretty <$> ts))
