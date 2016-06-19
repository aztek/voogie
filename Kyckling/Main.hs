module Main where

import System.Environment

import Kyckling.Parse
import Kyckling.Front
import Kyckling.Back
import Kyckling.TPTP.Pretty

main = do args <- getArgs
          let (source, input) = case args of
                                  []  -> ("<stdin>", getContents)
                                  f:_ -> (f        , readFile f)
          stream <- input
          case parseAST source stream of
            Left parsingError -> print parsingError
            Right ast -> case analyze ast of
                           Left typeError -> print typeError
                           Right code -> let tptp = translate code
                                          in putStr $ prettyTPTP tptp