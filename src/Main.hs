module Main(main) where

import Options.Applicative (execParser)

import System.IO

import Data.Maybe
import System.Exit

import Voogie.Error
import Voogie.CmdArgs
import Voogie.AST()
import Voogie.Boogie.Parse
import Voogie.Front
import Voogie.Back
import Voogie.TPTPretty()

import Text.PrettyPrint.ANSI.Leijen

collectOptions :: CmdArgs -> TranslationOptions
collectOptions cmdArgs = TranslationOptions (not $ noArrayTheory cmdArgs)

main :: IO ()
main = do
  cmdArgs <- execParser cmdArgsParserInfo
  let options = collectOptions cmdArgs
  let source = fromMaybe "<stdin>" (fileName cmdArgs)
  contents <- maybe getContents readFile (fileName cmdArgs)

  let printOutput :: Pretty a => Result a -> IO ()
      printOutput = \case
        Left error   -> hPutDoc stderr (pretty $ ErrorReport contents error) >> exitFailure
        Right result -> hPutDoc stdout (pretty result) >> exitSuccess

  let runParser = parseAST source contents
  let runAnalyzer = analyze
  let runTranslator = return . translate options

  case action cmdArgs of
    Parse     -> printOutput $ runParser
    Check     -> printOutput $ runParser >>= runAnalyzer
    Translate -> printOutput $ runParser >>= runAnalyzer >>= runTranslator
