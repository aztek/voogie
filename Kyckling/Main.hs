module Kyckling.Main where

import Program

p = Compose (If (Compare Leq (VarI "x") (I 2)) ("x" := I 10)) ("y" := (Op Plus (VarI "x") (I 5)))

main = putStrLn $ show p