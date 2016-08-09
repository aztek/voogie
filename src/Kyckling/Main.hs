module Main where

import System.Environment

import Kyckling.Program.Parse
import Kyckling.Program.Pretty
import Kyckling.Program.Optimize
import Kyckling.Front
import Kyckling.Back
import Kyckling.FOOL.TPTPretty

main = do args <- getArgs
          let (source, input) = case args of
                                  []  -> ("<stdin>", getContents)
                                  f:_ -> (f        , readFile f)
          stream <- input
          case parseAST source stream of
            Left parsingError -> print parsingError
            Right ast -> case analyze ast of
                           Left typeError -> putStrLn typeError
                           Right code -> putStr $ pretty (optimize code) --let fool = translate (optimize code)
                                         -- in putStr $ prettyTPTP fool