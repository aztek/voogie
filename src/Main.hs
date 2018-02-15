module Main(main) where

import Options.Applicative

import Data.Semigroup ((<>))
import Data.Maybe

import Voogie.Boogie.Parse
import Voogie.Boogie.Pretty()
import Voogie.Front
import Voogie.Back
import Voogie.TPTPretty

data CmdArgs = CmdArgs
  { fileName :: Maybe String
  , noArrayTheory :: Bool
  }

cmdArgsParser :: Parser CmdArgs
cmdArgsParser = CmdArgs
  <$> (fileName <|> stdIn)
  <*> noArrayTheory
  where
    fileName = Just <$> strArgument (metavar "FILE")
    stdIn = flag' Nothing
      $ long "stdin"
     <> help "Read from the standart input rather than a file"
    noArrayTheory = switch
      $ long "no_array_theory"
     <> help "Do not use polymorhic theory of arrays"

cmdArgsParserInfo :: ParserInfo CmdArgs
cmdArgsParserInfo = info (cmdArgsParser <**> helper)
  $ fullDesc
 <> header ("voogie - a verification conditions generator " ++
            "for simple Boogie programs")

collectOptions :: CmdArgs -> TranslationOptions
collectOptions cmdArgs = TranslationOptions (not $ noArrayTheory cmdArgs)

main :: IO ()
main = do
  cmdArgs <- execParser cmdArgsParserInfo
  let source = fromMaybe "<stdin>" (fileName cmdArgs)
  stream <- maybe getContents readFile (fileName cmdArgs)
  let runTranslation = putStr . prettyTPTP . translate (collectOptions cmdArgs)
  let analyzeInput = either putStrLn runTranslation . analyze
  either print analyzeInput (parseAST source stream)
