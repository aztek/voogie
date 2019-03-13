module Main(main) where

import Options.Applicative (execParser)

import System.IO (stdout, stderr)
import System.IO.Error (tryIOError)
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput, stdError)

import Data.Text
import qualified Data.Text.IO as TIO
import Data.Maybe
import System.Exit

import Voogie.Error
import Voogie.CmdArgs
import Voogie.Boogie.Parse (parseBoogie)
import Voogie.Front
import Voogie.Back
import Voogie.TPTP
import Voogie.TPTPretty()

import Text.PrettyPrint.ANSI.Leijen

collectOptions :: CmdArgs -> TranslationOptions
collectOptions cmdArgs = TranslationOptions (not $ noArrayTheory cmdArgs)

type Output = Either ErrorReport Doc

printOutput :: Output -> IO ()
printOutput = \case
  Left errorReport -> do
    istty <- queryTerminal stdError
    let pretty' = if istty then pretty else plain . pretty
    hPutDoc stderr (pretty' errorReport)
    exitFailure

  Right result -> do
    istty <- queryTerminal stdOutput
    let result' = if istty then result else plain result
    hPutDoc stdout result'
    exitSuccess

rewrapIOError :: Either IOError a -> (a -> Output) -> Output
rewrapIOError e f = fmapError (ErrorReport Nothing . InputOutputError) e >>= f

runVoogie :: CmdArgs -> Text -> Output
runVoogie cmdArgs contents = case action cmdArgs of
    Parse     -> buildOutput   runParser
    Check     -> buildOutput $ runParser >>= runAnalyzer
    Translate -> buildOutput $ runParser >>= runAnalyzer >>= runTranslator
  where
    buildOutput :: Pretty a => Result a -> Output
    buildOutput = fmapError (ErrorReport $ Just contents) . fmap pretty

    runParser = parseBoogie source contents
    runAnalyzer = analyze
    runTranslator = return . toTPTP . translate options

    options = collectOptions cmdArgs
    source = fromMaybe "<stdin>" (filePath cmdArgs)

main :: IO ()
main = do
  cmdArgs <- execParser cmdArgsParserInfo
  tryContents <- tryIOError $ maybe TIO.getContents TIO.readFile (filePath cmdArgs)
  printOutput . rewrapIOError tryContents $ runVoogie cmdArgs
