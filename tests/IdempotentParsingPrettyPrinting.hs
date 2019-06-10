{-|
Module       : Main
Description  : Unit tests of voogie.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Main (
  main
) where

import System.Exit (ExitCode(..))
import System.Process (system)

main :: IO ()
main = do
  ExitSuccess <- system "./tests/idempotent-parsing-pretty-printing.sh"
  return ()
