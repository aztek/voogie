module Main(main) where

import Options.Applicative (execParser)

import Data.Maybe
import System.Exit

import Voogie.Error
import Voogie.CmdArgs
import Voogie.Boogie.Parse
import Voogie.Front
import Voogie.Back
import Voogie.TPTPretty

import Text.PrettyPrint.ANSI.Leijen

collectOptions :: CmdArgs -> TranslationOptions
collectOptions cmdArgs = TranslationOptions (not $ noArrayTheory cmdArgs)

renderWithExit :: (Error -> IO ()) -> (a -> IO ()) -> Result a -> IO ()
renderWithExit f g = either ((>> exitFailure) . f) ((>> exitSuccess) . g)

main :: IO ()
main = do
  cmdArgs <- execParser cmdArgsParserInfo
  let options = collectOptions cmdArgs
  let source = fromMaybe "<stdin>" (fileName cmdArgs)
  contents <- maybe getContents readFile (fileName cmdArgs)

  let runParser = parseAST source contents
  let runAnalyzer = analyze
  let runTranslator = return . translate options

  let renderOutput = renderWithExit (renderError contents)

  let printAST = const (print "")
  let printBoogie = putDoc . pretty
  let printTPTP = putStrLn . prettyTPTP

  case action cmdArgs of
    Parse -> renderOutput printAST
           $ runParser
    Check -> renderOutput printBoogie
           $ runParser >>= runAnalyzer
    Translate -> renderOutput printTPTP
               $ runParser >>= runAnalyzer >>= runTranslator
