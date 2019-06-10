module Main (
  main
) where

import System.Exit (ExitCode(..))
import System.Process (system)

main :: IO ()
main = do
  ExitSuccess <- system "./tests/idempotent-parsing-pretty-printing.sh"
  return ()
