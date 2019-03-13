module Main (main) where

import Options.Applicative (execParser)

import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout, stderr)
import System.IO.Error (tryIOError)
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput, stdError)

import Data.Maybe
import Data.Text
import qualified Data.Text.IO as TIO

import Voogie.Error
import Voogie.CmdArgs
import Voogie.Boogie.Parse (parseBoogie)
import Voogie.Front
import Voogie.Back
import Voogie.TPTP
import Voogie.TPTPretty()

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

collectOptions :: CmdArgs -> TranslationOptions
collectOptions cmdArgs = TranslationOptions (not $ noArrayTheory cmdArgs)

printErrorReport :: Doc -> IO ()
printErrorReport e = do
  istty <- queryTerminal stdError
  hPutDoc stderr (if istty then e else plain e)
  exitFailure

printResult :: Doc -> IO ()
printResult r = do
  istty <- queryTerminal stdOutput
  hPutDoc stdout (if istty then r else plain r)
  exitSuccess

type Output = Either ErrorReport Doc

printOutput :: Output -> IO ()
printOutput = either (printErrorReport . pretty) printResult

reportIOError :: Either IOError a -> Either ErrorReport a
reportIOError = fmapError (ErrorReport Nothing . InputOutputError)

runVoogie :: CmdArgs -> Text -> Output
runVoogie cmdArgs contents = reportContents $ case action cmdArgs of
    Parse     -> pretty <$> runParser
    Check     -> pretty <$> (runParser >>= runAnalyzer)
    Translate -> pretty <$> (runParser >>= runAnalyzer >>= runTranslator)
  where
    source = fromMaybe "<stdin>" (filePath cmdArgs)
    options = collectOptions cmdArgs

    runParser = parseBoogie source contents
    runAnalyzer = analyze
    runTranslator = return . toTPTP . translate options

    reportContents = fmapError (ErrorReport $ Just contents)

main :: IO ()
main = do
  cmdArgs <- execParser cmdArgsParserInfo
  let input = maybe TIO.getContents TIO.readFile (filePath cmdArgs)
  tryContents <- tryIOError input
  printOutput $ reportIOError tryContents >>= runVoogie cmdArgs
