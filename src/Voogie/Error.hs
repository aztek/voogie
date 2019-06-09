{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Voogie.Error (
  Result, Error(..), ErrorReport(..), fmapError
) where

import Data.Bifunctor (bimap)
import Data.Text (Text)
import qualified Data.Text as Text (pack, unpack, lines, replicate, length, splitAt)

import Text.Parsec.Error (ParseError, errorPos, errorMessages, showErrorMessages)
import Text.Parsec.Pos (SourcePos, sourceName, sourceLine, sourceColumn)

import Voogie.AST
import Voogie.Pretty
import Voogie.Pretty.Boogie ()
import Voogie.Theory

type Result = Either Error

fmapError :: (e -> e') -> Either e a -> Either e' a
fmapError = flip bimap id

data Error
  = InputOutputError       IOError
  | ParsingError           ParseError
  | UndefinedVariable      (AST Name)
  | MultipleDefinitions    (AST (Typed Name))
  | TypeMismatch           (AST (Typed Doc)) Type
  | NonArraySelect         (AST (Typed Doc))
  | ArrayDimensionMismatch (AST (Typed Doc))
  deriving (Show)

instance Pretty Error where
  pretty = \case
    InputOutputError err ->
      text (show err)

    ParsingError err ->
         text "failed to parse"
     <+> text (showErrorMessages "or" "unknown parse error" "expecting"
                                 "unexpected" "end of input"
                               $ errorMessages err)

    UndefinedVariable (AST _ n) ->
      text "variable not in scope:" <+> bold (pretty n)

    MultipleDefinitions (AST _ t) ->
      text "variable redefined:" <+> renderTyped t

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
  InputOutputError                   _ -> Nothing
  ParsingError                     err -> Just (errorPos err, errorPos err)
  UndefinedVariable      (AST range _) -> Just range
  MultipleDefinitions    (AST range _) -> Just range
  TypeMismatch         (AST range _) _ -> Just range
  NonArraySelect         (AST range _) -> Just range
  ArrayDimensionMismatch (AST range _) -> Just range

data ErrorReport = ErrorReport (Maybe Text) Error

text' :: Text -> Doc
text' = text . Text.unpack

errorText :: Text -> Doc
errorText = bold . red . text'

instance Pretty ErrorReport where
  pretty (ErrorReport contents err) =
    bold pos <> errorText "error:" <+> pretty err <> hardline <> errorLine
    where
      pos = case errorRange err of
        Just (begin, _) -> pretty begin <> colon <> space
        _ -> empty

      errorLine = case (contents, errorRange err) of
        (Just c, Just range@(begin, _)) -> prettyErrorLine ln range <> hardline
          where ln = Text.lines c !! (sourceLine begin - 1)
        _ -> empty

prettyErrorLine :: Text -> ErrorRange -> Doc
prettyErrorLine errorLine (begin, end) =
    vsep $ zipWith (<>) lineMargin [
      empty,
      text' preToken <> errorText token <> text' postToken,
      errorText underlining
    ]
  where
    lineNumber = Text.pack . show $ sourceLine begin
    linePadding = Text.replicate (Text.length lineNumber) " "
    lineMargin = map (bold . blue . text' . (<> " | "))
                     [linePadding, lineNumber, linePadding]
    tokenBegin = sourceColumn begin - 1
    tokenEnd = if sourceLine end /= sourceLine begin || begin == end
               then Text.length errorLine else sourceColumn end - 1
    tokenLength = tokenEnd - tokenBegin
    (preToken, restLine) = Text.splitAt tokenBegin errorLine
    (token, postToken) = Text.splitAt tokenLength restLine
    underlining = Text.replicate tokenBegin " "
               <> Text.replicate tokenLength "^"
               <> if sourceLine end /= sourceLine begin then "..." else ""

instance Pretty SourcePos where
  pretty pos = text $ source ++ ":" ++ row ++ ":" ++ col
    where
      source = sourceName pos
      row = show (sourceLine pos)
      col = show (sourceColumn pos)
