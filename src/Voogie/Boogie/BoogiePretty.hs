module Voogie.Boogie.BoogiePretty() where

import qualified Data.List.NonEmpty as NE

import Voogie.Boogie
import Voogie.BoogiePretty
import Voogie.FOOL.BoogiePretty()

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

instance Pretty LValue where
  pretty (LValue v is) = pretty v <> hsep (tuple . fmap pretty <$> is)

instance Pretty Expression where
  pretty = \case
    IntegerLiteral i -> number i
    BooleanLiteral b -> boolean b
    Unary op e -> funapp1 (pretty op) (pretty e)
    Binary op a b -> pretty a <+> pretty op <+> pretty b
    IfElse a b c -> pretty a <+> punctuation "?" <+> pretty b
                             <+> punctuation ":" <+> pretty c
    FunApp f as -> case NE.nonEmpty as of
      Just as' -> funapp (pretty f) (pretty <$> as')
      Nothing  -> funapp1 (pretty f) empty
    Equals s a b -> pretty a <+> pretty s <+> pretty b
    Ref lv -> pretty lv

atomic :: [Doc] -> Doc
atomic ds = hsep ds <> punctuation ";"

marked :: String -> Doc -> Doc
marked k d = atomic [keyword k, d]

nested :: [Doc] -> Doc
nested d = nest 2 (line <> vsep d) <> line

block :: Pretty a => [a] -> Doc
block = braces . nested . fmap pretty

prettyIte :: Expression -> [Statement] -> [Statement] -> Doc
prettyIte c a b = hsep (thenBranch ++ elseBranch)
  where
    thenBranch = [keyword "if", parens (pretty c), block a]
    elseBranch = if null b then [] else [keyword "else", block b]

instance Pretty Statement where
  pretty = \case
    Assign pairs -> atomic [prettyLVs, operator ":=", prettyRVs]
      where
        (lvs, rvs) = NE.unzip pairs
        prettyLVs = commaSep (pretty <$> lvs)
        prettyRVs = commaSep (pretty <$> rvs)
    If c False a b -> prettyIte c (NE.toList a) b
    If c True  a b -> prettyIte c b (NE.toList a)

instance Pretty Assume where
  pretty (Assume f) = marked "assume" (pretty f)

instance Pretty TopLevel where
  pretty (Left stmt) = pretty stmt
  pretty (Right ass) = pretty ass

instance Pretty Main where
  pretty (Main modifies requires contents ensures) =
       keyword "procedure" <+> funapp1 (text "main") empty
    <> nested (prettyModifies : prettyPre ++ prettyPost)
    <> block contents
    where
      prettyModifies = case NE.nonEmpty modifies of
        Just m -> marked "modifies" (commaSep (pretty <$> m))
        Nothing -> empty
      prettyPre = marked "requires" . pretty <$> requires
      prettyPost = marked "ensures" . pretty <$> ensures

instance Pretty Boogie where
  pretty (Boogie vars main) = prettyVars <> line <> pretty main
    where
      prettyVars = vsep (prettyVarDecl <$> vars)
      prettyVarDecl t = marked "var" (prettyTyped t)
