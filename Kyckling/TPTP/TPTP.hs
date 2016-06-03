{-#LANGUAGE GADTs #-}

module TPTP.TPTP where

--data Tag = TFF | THF

data InputType = Axiom | Conjecture

instance Show InputType where
  show Axiom = "axiom"
  show Conjecture = "conjecture"

data TPTP where
  TPTP :: String -> InputType -> Term -> TPTP

instance Show TPTP where
  show (TPTP n it t) = "thf(" ++ n ++ ", " ++ show it ++ "," ++ show t ++ ")."

main = putStrLn "s"