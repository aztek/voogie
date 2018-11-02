{-# LANGUAGE GADTs #-}

module Main(main) where

import Options.Applicative (execParser)

import System.IO (stdout, stderr)
import System.IO.Error (tryIOError)
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput, stdError)

import Data.Maybe
import System.Exit

import Voogie.Error
import Voogie.CmdArgs
import Voogie.AST()
import Voogie.Boogie.Parse
import Voogie.Front
import Voogie.Back
import Voogie.TPTP
import Voogie.TPTPretty()

import Text.PrettyPrint.ANSI.Leijen

collectOptions :: CmdArgs -> TranslationOptions
collectOptions cmdArgs = TranslationOptions (not $ noArrayTheory cmdArgs)

data Output where
  Fail :: ErrorReport -> Output
  Success :: Pretty a => a -> Output

printOutput :: Output -> IO ()
printOutput = \case
  Fail errorReport -> do
    istty <- queryTerminal stdError
    let pretty' = if istty then pretty else plain . pretty
    hPutDoc stderr (pretty' errorReport)
    exitFailure

  Success result -> do
    istty <- queryTerminal stdOutput
    let pretty' = if istty then pretty else plain . pretty
    hPutDoc stdout (pretty' result)
    exitSuccess

rewrapIOError :: Either IOError a -> (a -> Output) -> Output
rewrapIOError (Left e) _ = Fail $ ErrorReport Nothing (InputOutputError e)
rewrapIOError (Right a) f = f a

runVoogie :: CmdArgs -> String -> Output
runVoogie cmdArgs contents = case action cmdArgs of
    Parse     -> buildOutput   runParser
    Check     -> buildOutput $ runParser >>= runAnalyzer
    Translate -> buildOutput $ runParser >>= runAnalyzer >>= runTranslator
  where
    buildOutput :: Pretty a => Result a -> Output
    buildOutput = \case
      Left error -> Fail $ ErrorReport (Just contents) error
      Right a -> Success a

    runParser = parseAST source contents
    runAnalyzer = analyze
    runTranslator = return . toTPTP . translate options

    options = collectOptions cmdArgs
    source = fromMaybe "<stdin>" (fileName cmdArgs)

main :: IO ()
main = do
  cmdArgs <- execParser cmdArgsParserInfo
  tryContents <- tryIOError $ maybe getContents readFile (fileName cmdArgs)
  printOutput . rewrapIOError tryContents $ runVoogie cmdArgs
