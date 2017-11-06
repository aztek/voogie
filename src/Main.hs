module Main where

import System.Environment

import Voogie.Boogie.Parse
import Voogie.Boogie.Pretty()
import Voogie.Front
import Voogie.Back
import Voogie.TPTPretty

parseArgs :: [String] -> (String, IO String)
parseArgs []    = ("<stdin>", getContents)
parseArgs (f:_) = (f        , readFile f)

main :: IO ()
main = do
  args <- getArgs
  let (source, input) = parseArgs args
  stream <- input
  case parseAST source stream of
    Left parsingError -> print parsingError
    Right ast -> case analyze ast of
      Left typeError -> putStrLn typeError
      Right code -> putStr (prettyTPTP fool)
        where opts = TranslationOptions True
              fool = translate opts code
