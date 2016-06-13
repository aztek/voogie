module Kyckling.Main where

import System.Environment

import Kyckling.Parse

main = do args <- getArgs
          let (source, input) = case args of
                                  []  -> ("<stdin>", getContents)
                                  f:_ -> (f,         readFile f)
          stream <- input
          case parseAST source stream of
            Left e -> putStrLn $ show e
            Right ast -> putStrLn $ show ast