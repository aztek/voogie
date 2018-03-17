module Main(main) where

import Options.Applicative (execParser)

import Data.Maybe
import System.Exit

import Voogie.Error
import Voogie.CmdArgs
import Voogie.Boogie.Parse
import Voogie.Boogie.BoogiePretty (pretty)
import Voogie.Front
import Voogie.Back
import Voogie.TPTPretty

collectOptions :: CmdArgs -> TranslationOptions
collectOptions cmdArgs = TranslationOptions (not $ noArrayTheory cmdArgs)

renderOutput' :: String -> (a -> String) -> Result a -> IO ()
renderOutput' contents _ (Left error) = renderError contents error >> exitFailure
renderOutput' _ render (Right a) = putStrLn (render a) >> exitSuccess

main :: IO ()
main = do
  cmdArgs <- execParser cmdArgsParserInfo
  let options = collectOptions cmdArgs
  let source = fromMaybe "<stdin>" (fileName cmdArgs)
  contents <- maybe getContents readFile (fileName cmdArgs)

  let runParser = parseAST source contents
  let runAnalyzer = analyze
  let runTranslator = return . translate options

  let renderOutput = renderOutput' contents

  case action cmdArgs of
    Parse -> renderOutput show runParser
    Check -> renderOutput pretty
           $ runParser >>= runAnalyzer
    Translate -> renderOutput prettyTPTP
               $ runParser >>= runAnalyzer >>= runTranslator
