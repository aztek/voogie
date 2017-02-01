module Main where

import System.Environment

import Voogie.Boogie.Parse
import Voogie.Boogie.Pretty
import Voogie.Front
import Voogie.Back
import Voogie.TPTPretty

main = do args <- getArgs
          let (source, input) = case args of
                                  []  -> ("<stdin>", getContents)
                                  f:_ -> (f        , readFile f)
          stream <- input
          case parseAST source stream of
            Left parsingError -> print parsingError
            Right ast -> case analyze ast of
                           Left typeError -> putStrLn typeError
                           Right code -> let fool = translate code
                                          in putStr $ prettyTPTP fool