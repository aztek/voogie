{-# LANGUAGE GADTs #-}

module Voogie.Error (
  Result, Error(..), renderError
) where

import Data.List

import Voogie.Theory
import Voogie.AST
import Voogie.BoogiePretty

import Text.Parsec
import Text.Parsec.Error

import System.IO
import Text.PrettyPrint.ANSI.Leijen hiding (pretty)

type Result = Either Error

data Error where
  ParsingError :: ParseError -> Error
  UndefinedVariable :: AST Name -> Error
  MultipleDefinitions :: AST (Typed Name) -> Error
  TypeMismatch :: BoogiePretty a => AST (Typed a) -> Type -> Error
  NonArraySelect :: BoogiePretty a => AST (Typed a) -> Error
  ArrayDimensionMismatch :: BoogiePretty a => AST (Typed a) -> Error

renderError :: String -> Error -> IO ()
renderError contents = hPutDoc stderr . \case
  ParsingError err -> renderError' (errorPos err, errorPos err)
    [text "failed to parse",
     text $ showErrorMessages "or" "unknown parse error" "expecting"
                              "unexpected" "end of input" (errorMessages err)]
  UndefinedVariable (AST pos v) -> renderError' pos
    [text "variable not in scope:", white (text v)]
  MultipleDefinitions (AST pos v) -> renderError' pos
    [text "variable redefined:", renderTyped v]
  TypeMismatch (AST pos a) t' -> renderError' pos
    [text "expected an expression of the type", white . text . pretty $ t',
     text "but got", renderTyped a]
  NonArraySelect (AST pos a) -> renderError' pos
    [text "expected an expression of an array type,",
     text "but got", renderTyped a]
  ArrayDimensionMismatch (AST pos a) -> renderError' pos
    [text "too many array indexes for", renderTyped a]
  where
    renderError' :: (SourcePos, SourcePos) -> [Doc] -> Doc
    renderError' pos@(begin, _) msgs =
         hsep (p:e:msgs) <> hardline <> hardline
      <> renderErrorLine (lines contents !! (sourceLine begin - 1)) pos
      where
        p = bold . white $ text (prettyPosition begin) <> colon
        e = bold . red $ text "error" <> colon

renderErrorLine :: String -> (SourcePos, SourcePos) -> Doc
renderErrorLine errorLine (begin, end) =
    text preToken
 <> (bold . red . text $ token)
 <> text postToken
 <> hardline
 <> (bold . red . text $ underlining)
 <> hardline
  where
    tokenBegin = sourceColumn begin - 1
    tokenEnd = if sourceLine end /= sourceLine begin
               then length errorLine else sourceColumn end - 1
    tokenLength = tokenEnd - tokenBegin
    (preToken, restLine) = splitAt tokenBegin errorLine
    (token, postToken) = splitAt tokenLength restLine
    underlining = replicate tokenBegin ' ' ++ replicate tokenLength '^' ++
                  if sourceLine end /= sourceLine begin then "..." else ""

renderTyped :: BoogiePretty a => Typed a -> Doc
renderTyped (Typed t a) = hsep [
    white . text . pretty $ a,
    text "of the type",
    white . text . pretty $ t
  ]

prettyPosition :: SourcePos -> String
prettyPosition pos = intercalate ":" [source, line, column]
  where
    source = sourceName pos
    line = show (sourceLine pos)
    column = show (sourceColumn pos)
