{-#LANGUAGE GADTs #-}

module Kyckling.TPTP.TPTP (
  InputType (Axiom, Conjecture),
  TPTP (TPTP)
) where

import Kyckling.TPTP.Term

--data Tag = TFF | THF

data InputType = Axiom | Conjecture

instance Show InputType where
  show Axiom = "axiom"
  show Conjecture = "conjecture"

data TPTP = TPTP String InputType Term

instance Show TPTP where
  show (TPTP n it t) = "thf(" ++ n ++ ", " ++ show it ++ "," ++ show t ++ ")."

main = putStrLn "s"