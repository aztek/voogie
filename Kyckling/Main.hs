module Kyckling.Main where

import System.Environment

import Kyckling.Parse
import Kyckling.Front
import Kyckling.Pretty

main = do args <- getArgs
          let (source, input) = case args of
                                  []  -> ("<stdin>", getContents)
                                  f:_ -> (f        , readFile f)
          stream <- input
          case parseAST source stream of
            Left parsingError -> print parsingError
            Right ast -> case analyze ast of
                           Left typeError -> print typeError
                           Right code -> putStrLn $ showProgram code