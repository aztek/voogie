{-# LANGUAGE LambdaCase #-}

{-|
Module       : Voogie.Pretty.Boogie.FOOL
Description  : Pretty printer of FOOL formulas in the Boogie syntax.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.Pretty.Boogie.FOOL (
  module Voogie.Pretty.Boogie
) where

import Voogie.FOOL hiding (tuple)
import Voogie.Pretty.Boogie

instance Pretty a => Pretty (Typed a) where
  pretty = pretty . valueOf

instance Pretty Var where
  pretty (Var v) = text v

isInfix :: Term -> Bool
isInfix = \case
  Binary{} -> True
  Equals{} -> True
  IfElse{} -> True
  _ -> False

prettyUnary :: Term -> Doc
prettyUnary = prettyParens isInfix

prettyBinary :: BinaryOp -> Term -> Doc
prettyBinary op = prettyParens $ \case
  Binary op' _ _ -> case op `comparePrecedence` op' of
    EQ | op == op' && isAssociative op -> False
    LT -> False
    _  -> True
  Equals{} -> comparePrecedenceEquality op /= GT
  t -> isInfix t

prettyEquals :: Term -> Doc
prettyEquals = prettyParens $ \case
  Binary op _ _ -> comparePrecedenceEquality op /= LT
  t -> isInfix t

instance Pretty Term where
  pretty = \case
    IntegerConstant i -> number i
    BooleanConstant b -> boolean b
    Variable        v -> pretty v
    Constant        f -> pretty f
    Application  f as -> funapp (pretty f) (pretty <$> as)
    Binary     op a b -> prettyBinary op a <+> pretty op <+> prettyBinary op b
    Unary      op t   -> pretty op <> prettyUnary t
    Quantify   q vs t -> parens $ pretty q <+> commaSep (prettyTyped <$> vs)
                     <+> punctuation opQsep <+> pretty t
    Equals      s a b -> prettyEquals a <+> pretty s <+> prettyEquals b
    IfElse      c a b -> keyword kwdIf   <+> pretty c
                     <+> keyword kwdThen <+> pretty a
                     <+> keyword kwdElse <+> pretty b
    Select       a is -> pretty a <> tuple (pretty <$> is)
    t@Store{}         -> error $ "Cannot represent store term " ++ show t ++
                                 " in the Boogie syntax."
    t@Let{}           -> error $ "Cannot represent let-expression " ++ show t ++
                                 " in the Boogie syntax."
    t@TupleLiteral{}  -> error $ "Cannot represent tuple literal " ++ show t ++
                                 " in the Boogie syntax."
