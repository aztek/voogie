{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Voogie.Boogie.BoogiePretty() where

import qualified Data.List.NonEmpty as NE (nonEmpty, toList, unzip)

import Voogie.Theory
import Voogie.Boogie
import Voogie.BoogieSyntax
import Voogie.BoogiePretty
import Voogie.FOOL.BoogiePretty()

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

instance Pretty LValue where
  pretty (LValue v is) = pretty v <> hcat (tuple . fmap pretty <$> is)

pretty' :: Expression -> Doc
pretty' e = case e of
  Binary{} -> parens (pretty e)
  Equals{} -> parens (pretty e)
  IfElse{} -> parens (pretty e)
  _ -> pretty e

instance Pretty Expression where
  pretty = \case
    IntegerLiteral i -> number i
    BooleanLiteral b -> boolean b
    Unary op e -> pretty op <> pretty' e
    Binary op a b -> pretty'' a <+> pretty op <+> pretty'' b
      where
        pretty'' e@(Binary op' _ _) = case op `comparePrecedence` op' of
          LT -> pretty e
          EQ | op == op' && isAssociative op -> pretty e
          _ -> parens (pretty e)
        pretty'' e = pretty' e
    IfElse c a b -> keyword kwdIf   <+> pretty c <+>
                    keyword kwdThen <+> pretty a <+>
                    keyword kwdElse <+> pretty b
    FunApp f as -> funapp (pretty f) (pretty <$> as)
    Equals s a b -> pretty'' a <+> pretty s <+> pretty'' b
      where
        pretty'' e@(Binary op _ _) = case comparePrecedenceEquality op of
          LT -> pretty e
          _ -> parens (pretty e)
        pretty'' e = pretty' e
    Ref lv -> pretty lv

atomic :: [Doc] -> Doc
atomic ds = hsep ds <> punctuation ";"

marked :: String -> Doc -> Doc
marked k d = atomic [keyword k, d]

nested :: [Doc] -> Doc
nested [] = empty
nested d = nest 2 (line <> vsep d) <> line

block :: Pretty a => [a] -> Doc
block = braces . nested . fmap pretty

prettyIte :: Expression -> [Statement] -> [Statement] -> Doc
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
    If c False a b -> prettyIte c (NE.toList a) b
    If c True  a b -> prettyIte c b (NE.toList a)

instance Pretty Property where
  pretty = \case
    Assume f -> marked kwdAssume (pretty f)
    Assert f -> marked kwdAssert (pretty f)

instance Pretty TopLevel where
  pretty = either pretty pretty

instance Pretty Main where
  pretty (Main modifies requires contents ensures) =
       keyword kwdProcedure <+> funapp1 (text kwdMain) empty
    <> nested (prettyModifies : prettyPre ++ prettyPost)
    <> block contents
    <> line
    where
      prettyModifies = case NE.nonEmpty modifies of
        Just m -> marked kwdModifies (commaSep (pretty <$> m))
        Nothing -> empty
      prettyPre = marked kwdRequires . pretty <$> requires
      prettyPost = marked kwdEnsures . pretty <$> ensures

instance Pretty Boogie where
  pretty (Boogie vars main) = prettyVars <> line <> pretty main
    where
      prettyVars = vsep (prettyVarDecl <$> vars)
      prettyVarDecl t = marked kwdVar (prettyTyped t)
