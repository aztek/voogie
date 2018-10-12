module Voogie.FOOL.TPTPretty() where

import Data.Char

import qualified Voogie.FOOL.Tuple as Tuple
import Voogie.FOOL
import Voogie.Theory
import Voogie.Pretty

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

instance Pretty BinaryOp where
  pretty = \case
    And      -> operator "&"
    Or       -> operator "|"
    Imply    -> operator "=>"
    Iff      -> operator "<=>"
    Xor      -> operator "<~>"
    Greater  -> builtin "$greater"
    Less     -> builtin "$less"
    Geq      -> builtin "$greatereq"
    Leq      -> builtin "$lesseq"
    Add      -> builtin "$sum"
    Subtract -> builtin "$difference"
    Multiply -> builtin "$product"
    Divide   -> builtin "$quotient_e"

isInfix :: BinaryOp -> Bool
isInfix = \case
  And      -> True
  Or       -> True
  Imply    -> True
  Iff      -> True
  Xor      -> True
  Greater  -> False
  Less     -> False
  Geq      -> False
  Leq      -> False
  Add      -> False
  Subtract -> False
  Multiply -> False
  Divide   -> False

instance Pretty UnaryOp where
  pretty = \case
    Negate   -> operator "~"
    Positive -> builtin "$uplus"
    Negative -> builtin "$uminus"

isPrefix :: UnaryOp -> Bool
isPrefix = \case
  Negate   -> True
  Positive -> False
  Negative -> False

instance Pretty Type where
  pretty = \case
    Boolean -> builtin "$o"
    Integer -> builtin "$int"
    Array i t -> foldr (funapp2 $ builtin "$array") (pretty t) (pretty <$> i)
    TupleType ts -> tuple (Tuple.toNonEmpty (pretty <$> ts))
    Functional ts t -> ts' <+> operator ">" <+> pretty t
      where ts' = sepBy (space <> operator "*" <> space) (pretty <$> ts)
    Custom n -> text n

instance Pretty Var where
  pretty (Var []) = error "empty variable name"
  pretty (Var (c:cs)) = text (toUpper c : cs)

instance Pretty a => Pretty (Typed a) where
  pretty (Typed t s) = pretty s <> punctuation ":" <+> pretty t

prettyIdentifier :: Identifier -> Doc
prettyIdentifier (Typed _ []) = error "empty identifier"
prettyIdentifier (Typed _ (c:cs)) = text (toLower c : cs)

instance Pretty Definition where
  pretty = \case
    ConstantSymbol c -> prettyIdentifier c
    Function c vs -> funapp (prettyIdentifier c) (pretty <$> vs)
    TupleD es -> tuple (Tuple.toNonEmpty (prettyIdentifier <$> es))

instance Pretty Binding where
  pretty (Binding d t) = pretty d <+> operator ":=" <+> pretty t

instance Pretty Quantifier where
  pretty = operator . \case
    Forall -> "!"
    Exists -> "?"

instance Pretty Sign where
  pretty = operator . \case
    Pos -> "="
    Neg -> "!="

instance Pretty Term where
  pretty = \case
    IntegerConstant n -> number n
    BooleanConstant True -> builtin "$true"
    BooleanConstant False -> builtin "$false"

    Variable (Typed _ v) -> pretty v
    Constant f -> prettyIdentifier f
    Application f as -> funapp (prettyIdentifier f) (pretty <$> as)

    Unary op a | isPrefix op -> pretty op <> pretty a
               | otherwise   -> funapp1 (pretty op) (pretty a)

    Binary op a b | isInfix op -> parens (pretty a <+> pretty op <+> pretty b)
                  | otherwise  -> funapp2 (pretty op) (pretty a) (pretty b)

    Quantify q vs t -> parens $ pretty q <> tuple (pretty <$> vs)
                             <> punctuation ":" <+> parens (pretty t)

    Equals s a b -> parens (pretty a <+> pretty s <+> pretty b)

    Let b@(Binding d _) t -> builtin "$let" <> parens args
      where
        args = case t of
          Let{} -> align definition <> line <> pretty t
          _     -> align (definition <> line <> pretty t)
        definition = typeSignature <> comma <> line <> pretty b <> comma
        typeSignature = case d of
          ConstantSymbol c -> pretty c
          Function c _ -> pretty c
          TupleD es -> tuple (Tuple.toNonEmpty (pretty <$> es))

    If c a b -> funapp3 (builtin "$ite")
                        (pretty c)
                        (line <> pretty a)
                        (line <> pretty b)

    Select a i   -> funapp2 (builtin "$select") (pretty a) (pretty i)
    Store  a i e -> funapp3 (builtin "$store")  (pretty a) (pretty i) (pretty e)

    TupleLiteral ts -> tuple (Tuple.toNonEmpty (pretty <$> ts))
