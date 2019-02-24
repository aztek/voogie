{-# LANGUAGE GADTs #-}

module Voogie.Error (
  Result, Error(..), ErrorReport(..)
) where

import Voogie.Theory
import Voogie.AST
import Voogie.BoogiePretty()

import Text.Parsec.Pos
import Text.Parsec.Error

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

type Result = Either Error

data Error where
  InputOutputError :: IOError -> Error
  ParsingError :: ParseError -> Error
  UndefinedVariable :: Named a => AST a -> Error
  MultipleDefinitions :: Named a => AST (Typed a) -> Error
  TypeMismatch :: Pretty a => AST (Typed a) -> Type -> Error
  NonArraySelect :: Pretty a => AST (Typed a) -> Error
  ArrayDimensionMismatch :: Pretty a => AST (Typed a) -> Error

instance Pretty Error where
  pretty = \case
    InputOutputError ioError ->
      text (show ioError)

    ParsingError err ->
         text "failed to parse"
     <+> text (showErrorMessages "or" "unknown parse error" "expecting"
                                 "unexpected" "end of input"
                               $ errorMessages err)

    UndefinedVariable (AST _ v) ->
      text "variable not in scope:" <+> bold (pretty $ nameOf v)

    MultipleDefinitions (AST _ v) ->
      text "variable redefined:" <+> renderTyped (nameOf <$> v)

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

errorRange :: Error -> Maybe ErrorRange
errorRange = \case
  InputOutputError _ -> Nothing
  ParsingError err -> Just (errorPos err, errorPos err)
  UndefinedVariable (AST range _) -> Just range
  MultipleDefinitions (AST range _) -> Just range
  TypeMismatch (AST range _) _ -> Just range
  NonArraySelect (AST range _) -> Just range
  ArrayDimensionMismatch (AST range _) -> Just range

errorText = bold . red . text

data ErrorReport = ErrorReport (Maybe String) Error

instance Pretty ErrorReport where
  pretty (ErrorReport contents error) =
    bold pos <> errorText "error:" <+> pretty error <> hardline <> errorLine
    where
      pos = case errorRange error of
        Just (begin, _) -> pretty begin <> colon <> space
        _ -> empty

      errorLine = case (contents, errorRange error) of
        (Just c, Just range@(begin, _)) -> prettyErrorLine line range <> hardline
          where line = lines c !! (sourceLine begin - 1)
        _ -> empty

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

instance Pretty SourcePos where
  pretty pos = text $ source ++ ":" ++ line ++ ":" ++ column
    where
      source = sourceName pos
      line = show (sourceLine pos)
      column = show (sourceColumn pos)
