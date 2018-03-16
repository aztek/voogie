module Main(main) where

import Options.Applicative (execParser)

import Data.Maybe

import Voogie.Error
import Voogie.CmdArgs
import Voogie.Boogie.Parse
import Voogie.Boogie.BoogiePretty (pretty)
import Voogie.Front
import Voogie.Back
import Voogie.TPTPretty

collectOptions :: CmdArgs -> TranslationOptions
collectOptions cmdArgs = TranslationOptions (not $ noArrayTheory cmdArgs)

main :: IO ()
main = do
  cmdArgs <- execParser cmdArgsParserInfo
  let options = collectOptions cmdArgs
  let source = fromMaybe "<stdin>" (fileName cmdArgs)
  stream <- maybe getContents readFile (fileName cmdArgs)

  let ast = parseAST source stream
  let runParser f = either renderError f ast
  let runAnalyzer f = either renderError f . analyze
  let runTranslator = translate options

  putStrLn $ case action cmdArgs of
    Parse -> runParser show
    Check -> runParser . runAnalyzer $ pretty
    Translate -> runParser . runAnalyzer $ (prettyTPTP . runTranslator)
