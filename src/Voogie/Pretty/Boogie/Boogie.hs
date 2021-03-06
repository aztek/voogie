{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module       : Voogie.Pretty.Boogie.Boogie
Description  : Pretty printer of Boogie programs in the Boogie syntax.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.Pretty.Boogie.Boogie (
  module Voogie.Pretty.Boogie
) where

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE (nonEmpty, unzip)

import Voogie.Boogie hiding (tuple)
import Voogie.Pretty.Boogie
import Voogie.Pretty.Boogie.FOOL ()

instance Pretty LValue where
  pretty (LValue v is) = pretty v <> hcat (tuple . fmap pretty <$> is)

isInfix :: Expression -> Bool
isInfix = \case
  Binary{} -> True
  Equals{} -> True
  IfElse{} -> True
  _ -> False

prettyUnary :: Expression -> Doc
prettyUnary = prettyParens isInfix

prettyBinary :: BinaryOp -> Expression -> Doc
prettyBinary op = prettyParens $ \case
  Binary op' _ _ -> case op `comparePrecedence` op' of
    EQ | op == op' && isAssociative op -> False
    LT -> False
    _  -> True
  Equals{} -> comparePrecedenceEquality op /= GT
  e -> isInfix e

prettyEquals :: Expression -> Doc
prettyEquals = prettyParens $ \case
  Binary op _ _ -> comparePrecedenceEquality op /= LT
  e -> isInfix e

instance Pretty Expression where
  pretty = \case
    IntegerLiteral i -> number i
    BooleanLiteral b -> boolean b
    Application f as -> funapp (pretty f) (pretty <$> as)
    Unary     op   e -> pretty op <> prettyUnary e
    Binary    op a b -> prettyBinary op a <+> pretty op <+> prettyBinary op b
    Equals     s a b -> prettyEquals a <+> pretty s <+> prettyEquals b
    Ref           lv -> pretty lv
    IfElse     c a b -> keyword kwdIf   <+> pretty c
                    <+> keyword kwdThen <+> pretty a
                    <+> keyword kwdElse <+> pretty b

atomic :: Foldable t => t Doc -> Doc
atomic ds = hsep (toList ds) <> punctuation ";"

marked :: Name -> Doc -> Doc
marked k d = atomic [keyword k, d]

nested :: Foldable t => t Doc -> Doc
nested ds = case toList ds of
  [] -> empty
  dl -> nest 2 (line <> vsep dl) <> line

block :: (Foldable t, Functor t, Pretty a) => t a -> Doc
block = braces . nested . fmap pretty

prettyIte :: (Foldable t1, Functor t1, Foldable t2, Functor t2)
          => Expression -> t1 Statement -> t2 Statement -> Doc
prettyIte c a b = hsep (thenBranch ++ elseBranch)
  where
    thenBranch = [keyword kwdIf, parens (pretty c), block a]
    elseBranch = if null b then [] else [keyword kwdElse, block b]

instance Pretty Statement where
  pretty = \case
    Assign pairs -> atomic [prettyLVs, operator opAssign, prettyRVs]
      where
        (lvs, rvs) = NE.unzip pairs
        prettyLVs = commaSep (pretty <$> lvs)
        prettyRVs = commaSep (pretty <$> rvs)
    If c False a b -> prettyIte c a b
    If c True  a b -> prettyIte c b a

instance Pretty Property where
  pretty = \case
    Assume f -> marked kwdAssume (pretty f)
    Assert f -> marked kwdAssert (pretty f)

instance Pretty TopLevel where
  pretty = either pretty pretty

instance Pretty Main where
  pretty (Main ms rs c es) =
       keyword kwdProcedure <+> funapp1 (text kwdMain) empty
    <> nested (prettyModifies : prettyPre ++ prettyPost)
    <> block c
    <> line
    where
      prettyModifies = case NE.nonEmpty ms of
        Just m -> marked kwdModifies (commaSep (pretty <$> m))
        Nothing -> empty
      prettyPre = marked kwdRequires . pretty <$> rs
      prettyPost = marked kwdEnsures . pretty <$> es

instance Pretty Boogie where
  pretty (Boogie vars main) = prettyVars <> line <> pretty main
    where
      prettyVars = vsep (prettyVarDecl <$> vars)
      prettyVarDecl t = marked kwdVar (prettyTyped t)
