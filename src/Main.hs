module Main(main) where

import Options.Applicative

import Data.Semigroup ((<>))
import Data.Maybe

import Voogie.Boogie.Parse
import Voogie.Boogie.BoogiePretty (pretty)
import Voogie.Front
import Voogie.Back
import Voogie.TPTPretty

import Paths_Voogie (version)
import Data.Version (showVersion)

data Action = Parse | Check | Translate
  deriving (Eq, Show, Enum, Bounded)

actions = [("parse", Parse), ("check", Check), ("translate", Translate)]

data CmdArgs = CmdArgs
  { fileName :: Maybe String
  , action :: Action
  , noArrayTheory :: Bool
  }

cmdArgsParser :: Parser CmdArgs
cmdArgsParser = CmdArgs
  <$> (fileName <|> stdIn)
  <*> action
  <*> noArrayTheory
  where
    fileName = Just <$> strArgument (metavar "FILE")
    stdIn = flag' Nothing
      $ long "stdin"
     <> help "Read from the standart input rather than a file"
    action = option (maybeReader $ \s -> lookup s actions)
      $ long "action"
     <> metavar "ACTION"
     <> value Translate
     <> help ("Action to perform, can be one of the following: " ++
              "parse, check, translate (default)")
    noArrayTheory = switch
      $ long "no_array_theory"
     <> help "Do not use polymorhic theory of arrays"

cmdArgsParserInfo :: ParserInfo CmdArgs
cmdArgsParserInfo = info (cmdArgsParser <**> versionOption <**> helper)
  $ fullDesc
 <> header ("Voogie - a verification conditions generator " ++
            "for simple Boogie programs")
  where
    versionOption = abortOption (InfoMsg $ showVersion version)
      $ long "version"
     <> short 'v'
     <> help "Display the version number"
     <> hidden

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

  case Main.action cmdArgs of
    Parse -> runParser print
    Check -> runParser . runAnalyzer $ (putStr . pretty)
    Translate -> runParser . runAnalyzer $ (putStr . prettyTPTP . runTranslator)
