module Main(main) where

import Options.Applicative (execParser)

import Data.Maybe

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

  let runParser f = either print f (parseAST source stream)
  let runAnalyzer f = either putStrLn f . analyze
  let runTranslator = translate options

  case action cmdArgs of
    Parse -> runParser print
    Check -> runParser . runAnalyzer $ (putStr . pretty)
    Translate -> runParser . runAnalyzer $ (putStr . prettyTPTP . runTranslator)
