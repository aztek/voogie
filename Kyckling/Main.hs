module Kyckling.Main where

import System.Environment

import Kyckling.Parse
import Kyckling.Front

main = do args <- getArgs
          let (source, input) = case args of
                                  []  -> ("<stdin>", getContents)
                                  f:_ -> (f        , readFile f)
          stream <- input
          let program = do ast <- parseAST source stream
                           return $ analyze ast
          case program of
            Left  err  -> print err
            Right code -> print code