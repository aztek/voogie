{-# LANGUAGE LambdaCase #-}

{-|
Module       : Voogie.Pretty.TPTP.FOOL
Description  : Pretty printer of FOOL formulas in the TPTP syntax.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.Pretty.TPTP.FOOL (
  module Voogie.Pretty
) where

import Data.Char (toUpper, toLower)
import qualified Data.List.NonEmpty as NE (nonEmpty)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonUnit as NonUnit

import Voogie.FOOL hiding (tuple)
import Voogie.Pretty
import Voogie.TPTP.Syntax

instance Pretty BinaryOp where
  pretty op
    | isInfix op = operator (nameOf op)
    | otherwise  = builtin  (nameOf op)

instance Pretty UnaryOp where
  pretty op
    | isPrefix op = operator (nameOf op)
    | otherwise   = builtin  (nameOf op)

instance Pretty Type where
  pretty = \case
    Boolean         -> builtin boolName
    Integer         -> builtin intName
    Custom        n -> text n
    Array       i t -> foldr (funapp2 $ builtin arrayName) (pretty t) (pretty <$> i)
    Tuple        ts -> tuple (NonUnit.toNonEmpty (pretty <$> ts))
    Functional ts t -> sepBy (space <> operator "*" <> space) (pretty <$> ts)
                   <+> operator ">" <+> pretty t

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
    Function    c vs -> funapp (prettyIdentifier c) (pretty <$> vs)
    TupleD        es -> tuple (NonUnit.toNonEmpty (prettyIdentifier <$> es))

instance Pretty Binding where
  pretty (Binding d t) = pretty d <+> operator opAssign <+> pretty t

instance Pretty Quantifier where
  pretty = operator . nameOf

instance Pretty Sign where
  pretty = operator . nameOf

isUnitary :: Term -> Bool
isUnitary = \case
  Binary op _ _ | isInfix  op -> False
  Unary  op _   | isPrefix op -> False
  Equals{}   -> False
  Quantify{} -> False
  _          -> True

pretty' :: Term -> Doc
pretty' t
  | isUnitary t = pretty t
  | otherwise   = parens (pretty t)

select :: Doc -> Doc -> Doc
select = funapp2 (builtin kwdSelect)

store :: Doc -> Doc -> Doc -> Doc
store = funapp3 (builtin kwdStore)

instance Pretty Term where
  pretty = \case
    IntegerConstant n -> number n
    BooleanConstant b -> builtin (nameOf b)
    Variable        v -> pretty (valueOf v)
    Constant        f -> prettyIdentifier f
    Equals      s a b -> pretty' a <+> pretty s <+> pretty' b
    Application  f as -> funapp (prettyIdentifier f) (pretty <$> as)
    Select     a is   -> foldl select (pretty a) (pretty <$> is)
    Store      a is e -> foldStore select store (pretty a) (pretty <$> is) (pretty e)
    IfElse      c a b -> funapp3 (builtin kwdIf) (pretty c) (line <> pretty a) (line <> pretty b)
    Quantify   q vs t -> pretty q <> tuple (pretty <$> vs) <> punctuation ":" <+> pretty' t
    TupleLiteral   ts -> tuple (NonUnit.toNonEmpty (pretty <$> ts))

    Unary op a
      | isPrefix   op -> pretty op <> pretty' a
      | otherwise     -> funapp1 (pretty op) (pretty' a)

    Binary op a b
      | isInfix    op -> pretty'' a <+> pretty op <+> pretty'' b
      | otherwise     -> funapp2 (pretty op) (pretty' a) (pretty' b)
      where
        pretty'' t = case t of
          Binary op' _ _ | op == op' && isAssociative op -> pretty t
          _ -> pretty' t

    Let b@(Binding d _) t -> builtin kwdLet <> parens args
      where
        args = case t of
          Let{} -> align definition <> line <> pretty t
          _     -> align (definition <> line <> pretty t)

        definition = prettySignature d <> comma <> line <> pretty b <> comma

prettySignature :: Definition -> Doc
prettySignature = \case
  ConstantSymbol c -> pretty c
  Function     c _ -> pretty c
  TupleD        es -> tuple (NonUnit.toNonEmpty (pretty <$> es))
