module Main where

import System.Environment

import Data.Maybe

import Voogie.Boogie.Parse
import Voogie.Boogie.Pretty()
import Voogie.Front
import Voogie.Back
import Voogie.TPTPretty

data Input = Input
  { fileName :: Maybe String
  , options :: TranslationOptions
  }

defaultOptions :: TranslationOptions
defaultOptions = TranslationOptions True

parseArgs :: [String] -> Input
parseArgs args = Input (listToMaybe args) defaultOptions

main :: IO ()
main = do
  args <- getArgs
  let input = parseArgs args
  let source = fromMaybe "<stdin>" (fileName input)
  stream <- maybe getContents readFile (fileName input)
  let runTranslation = putStr . prettyTPTP . translate (options input)
  let analyzeInput = either putStrLn runTranslation . analyze
  either print analyzeInput (parseAST source stream)
