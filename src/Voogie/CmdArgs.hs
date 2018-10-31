module Voogie.CmdArgs (
  Action(..), CmdArgs(..), cmdArgsParserInfo
) where

import Options.Applicative

import Data.Semigroup ((<>))

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

parser :: Parser CmdArgs
parser = CmdArgs
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
cmdArgsParserInfo = info (parser <**> versionOption <**> helper)
  $ fullDesc
 <> header ("Voogie - a verification conditions generator " ++
            "for simple Boogie programs")
  where
    versionOption = abortOption (InfoMsg $ showVersion version)
      $ long "version"
     <> short 'v'
     <> help "Display the version number"
     <> hidden