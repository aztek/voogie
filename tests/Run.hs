module Kyckling.Test.Run where

import Test.QuickCheck

import Control.Monad
import Data.List

import Kyckling.Program

run :: Int -> IO ()
run 0 = return ()
run n = do p <- generate arbitrary :: IO Program
           putStrLn $ show p
           run (n - 1)

main = run 10