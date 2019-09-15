{-# LANGUAGE CPP #-}

{-|
Module       : CmdArgs
Description  : Command line options of the voogie executable.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module CmdArgs (
  Action(..),
  CmdArgs(..),
  cmdArgsParserInfo
) where

import Control.Applicative ((<|>))

import Options.Applicative (
    Parser, ParserInfo, ParseError(..),
    headerDoc, fullDesc, long, short, info, helper, help, hidden, switch, value,
    maybeReader, metavar, (<**>), option, abortOption, strArgument, flag'
  )

import Options.Applicative.Help.Pretty (Doc, vsep, text)

#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup ((<>))
#endif

import Paths_voogie (version)
import Data.Version (showVersion)

data Action
  = Parse
  | Check
  | Translate
  deriving (Show, Eq, Ord, Enum, Bounded)

actions :: [(String, Action)]
actions = [("parse", Parse), ("check", Check), ("translate", Translate)]

data CmdArgs = CmdArgs {
  filePath      :: Maybe FilePath,
  action        :: Action,
  noArrayTheory :: Bool
} deriving (Show, Eq, Ord)

parser :: Parser CmdArgs
parser =  CmdArgs
      <$> filePathOption
      <*> actionOption
      <*> noArrayTheoryOption
  where
    filePathOption = file <|> stdIn
    file = Just <$> strArgument (metavar "FILE")
    stdIn = flag' Nothing
      $ long "stdin"
     <> help "Read from the standart input rather than a file"
    actionOption = option (maybeReader $ \s -> lookup s actions)
      $ long "action"
     <> metavar "ACTION"
     <> value Translate
     <> help ("Action to perform, can be one of the following: " ++
              "parse, check, translate (default)")
    noArrayTheoryOption = switch
      $ long "no_array_theory"
     <> help "Do not use polymorhic theory of arrays"

banner :: Doc
banner = vsep $ fmap text [
    "                         _      ",
    " __   _____   ___   __ _(_) ___ ",
    " \\ \\ / / _ \\ / _ \\ / _` | |/ _ \\",
    "  \\ V / (_) | (_) | (_| | |  __/",
    "   \\_/ \\___/ \\___/ \\__, |_|\\___|",
    "                   |___/        ",
    "",
    "Voogie - a verification conditions generator for simple Boogie programs"
  ]

cmdArgsParserInfo :: ParserInfo CmdArgs
cmdArgsParserInfo = info (parser <**> versionOption <**> helper)
  $ fullDesc
 <> headerDoc (Just banner)
  where
    versionOption = abortOption (InfoMsg $ showVersion version)
      $ long "version"
     <> short 'v'
     <> help "Display the version number"
     <> hidden
