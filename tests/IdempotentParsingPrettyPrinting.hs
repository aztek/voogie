module Main where

import System.Exit
import System.Process

main = do
  ExitSuccess <- system "./tests/idempotent-parsing-pretty-printing.sh"
  return ()
