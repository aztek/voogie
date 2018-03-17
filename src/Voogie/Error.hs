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
import System.Console.ANSI

type Result = Either Error

data Error where
  ParsingError :: ParseError -> Error
  UndefinedVariable :: AST Name -> Error
  MultipleDefinitions :: AST (Typed Name) -> Error
  TypeMismatch :: BoogiePretty a => AST (Typed a) -> Type -> Error
  NonArraySelect :: BoogiePretty a => AST (Typed a) -> Error
  ArrayDimensionMismatch :: BoogiePretty a => AST (Typed a) -> Error

renderError :: String -> Error -> IO ()
renderError contents error = case error of
  ParsingError err -> renderError' (errorPos err, errorPos err)
    [regular "failed to parse",
     regular $ showErrorMessages "or" "unknown parse error" "expecting"
                                 "unexpected" "end of input" (errorMessages err)]
  UndefinedVariable (AST pos v) -> renderError' pos
    [regular "variable not in scope:", white v]
  MultipleDefinitions (AST pos v) -> renderError' pos
    [regular "variable redefined:", renderTyped v]
  TypeMismatch (AST pos a) t' -> renderError' pos
    [regular "expected an expression of the type", white $ pretty t',
     regular "but got", renderTyped a]
  NonArraySelect (AST pos a) -> renderError' pos
    [regular "expected an expression of an array type,",
     regular "but got", renderTyped a]
  ArrayDimensionMismatch (AST pos a) -> renderError' pos
    [regular "too many array indexes for", renderTyped a]
  where
    renderError' :: (SourcePos, SourcePos) -> [IO ()] -> IO ()
    renderError' pos@(begin, _) msgs = do
      renderPosition begin
      bold . red $ " error: "
      mapM_ (>> regular " ") msgs
      regular "\n\n"
      renderErrorLine (lines contents !! (sourceLine begin - 1)) pos

renderErrorLine :: String -> (SourcePos, SourcePos) -> IO ()
renderErrorLine errorLine (begin, end) = do
  regular preToken
  bold . red $ token
  regular postToken
  regular "\n"
  bold . red $ replicate tokenBegin ' ' ++ replicate tokenLength '^' ++
               if sourceLine end /= sourceLine begin then "..." else ""
  regular "\n"
  where
    tokenBegin = sourceColumn begin - 1
    tokenEnd = if sourceLine end /= sourceLine begin
               then length errorLine else sourceColumn end - 1
    tokenLength = tokenEnd - tokenBegin
    (preToken, restLine) = splitAt tokenBegin errorLine
    (token, postToken) = splitAt tokenLength restLine

renderTyped :: BoogiePretty a => Typed a -> IO ()
renderTyped (Typed t a) = do
  white (pretty a)
  regular " of the type "
  white (pretty t)

renderPosition :: SourcePos -> IO ()
renderPosition pos = bold . white $ intercalate ":" [source, line, column] ++ ":"
  where
    source = sourceName pos
    line = show (sourceLine pos)
    column = show (sourceColumn pos)

white :: String -> IO ()
white = colored White

red :: String -> IO ()
red = colored Red

colored :: Color -> String -> IO ()
colored color = sgr (SetColor Foreground Vivid color) . regular

bold :: IO () -> IO ()
bold = sgr (SetConsoleIntensity BoldIntensity)

regular :: String -> IO ()
regular = hPutStr stderr

sgr :: SGR -> IO () -> IO ()
sgr s io = do
  hSetSGR stderr [s]
  io
  hSetSGR stderr [Reset]
