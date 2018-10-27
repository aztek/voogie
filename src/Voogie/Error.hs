{-# LANGUAGE GADTs #-}

module Voogie.Error (
  Result, Error(..), ErrorReport(..)
) where

import Voogie.Theory
import Voogie.AST
import Voogie.BoogiePretty()

import Text.Parsec
import Text.Parsec.Error

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

type Result = Either Error

data Error where
  ParsingError :: ParseError -> Error
  UndefinedVariable :: AST Name -> Error
  MultipleDefinitions :: AST (Typed Name) -> Error
  TypeMismatch :: Pretty a => AST (Typed a) -> Type -> Error
  NonArraySelect :: Pretty a => AST (Typed a) -> Error
  ArrayDimensionMismatch :: Pretty a => AST (Typed a) -> Error

instance Pretty Error where
  pretty = \case
    ParsingError err ->
         text "failed to parse"
     <+> text (showErrorMessages "or" "unknown parse error" "expecting"
                                 "unexpected" "end of input"
                               $ errorMessages err)

    UndefinedVariable (AST _ v) ->
      text "variable not in scope:" <+> bold (pretty v)

    MultipleDefinitions (AST _ v) ->
      text "variable redefined:" <+> renderTyped v

    TypeMismatch (AST _ a) t ->
          text "expected an expression of the type" <+> bold (pretty t)
      <+> text "but got" <+> renderTyped a

    NonArraySelect (AST _ a) ->
          text "expected an expression of an array type,"
      <+> text "but got" <+> renderTyped a

    ArrayDimensionMismatch (AST _ a) ->
      text "too many array indexes for" <+> renderTyped a

renderTyped :: Pretty a => Typed a -> Doc
renderTyped (Typed t a) =
  bold (pretty a) <+> text "of the type" <+> bold (pretty t)

type ErrorRange = (SourcePos, SourcePos)

errorRange :: Error -> ErrorRange
errorRange = \case
  ParsingError err -> (errorPos err, errorPos err)
  UndefinedVariable (AST range _) -> range
  MultipleDefinitions (AST range _) -> range
  TypeMismatch (AST range _) _ -> range
  NonArraySelect (AST range _) -> range
  ArrayDimensionMismatch (AST range _) -> range

data ErrorReport = ErrorReport String Error

instance Pretty ErrorReport where
  pretty (ErrorReport contents error) =
       bold (pos <+> red (text "error" <> colon)) <+> pretty error
    <> hardline
    <> prettyErrorLine errorLine range
    <> hardline
    where
      range@(begin, _) = errorRange error
      errorLine = lines contents !! (sourceLine begin - 1)
      pos = bold (pretty begin <> colon)

prettyErrorLine :: String -> ErrorRange -> Doc
prettyErrorLine errorLine (begin, end) =
    vsep $ zipWith (<>) lineMargin [
      empty,
      text preToken <> errorText token <> text postToken,
      errorText underlining
    ]
  where
    lineNumber = show (sourceLine begin)
    linePadding = replicate (length lineNumber) ' '
    lineMargin = map (bold . blue . text . (++ " | "))
                     [linePadding, lineNumber, linePadding]
    tokenBegin = sourceColumn begin - 1
    tokenEnd = if sourceLine end /= sourceLine begin || begin == end
               then length errorLine else sourceColumn end - 1
    tokenLength = tokenEnd - tokenBegin
    (preToken, restLine) = splitAt tokenBegin errorLine
    (token, postToken) = splitAt tokenLength restLine
    underlining = replicate tokenBegin ' ' ++ replicate tokenLength '^' ++
                  if sourceLine end /= sourceLine begin then "..." else ""
    errorText = bold . red . text

instance Pretty SourcePos where
  pretty pos = text $ source ++ ":" ++ line ++ ":" ++ column
    where
      source = sourceName pos
      line = show (sourceLine pos)
      column = show (sourceColumn pos)
