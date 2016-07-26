module Kyckling.Program.Optimize where

import Kyckling.Program

optimize :: Program -> Program
optimize (Program fs ss as) = Program (map optimizeFunDef fs) (optimizeNonTerminating ss) as

optimizeFunDef :: FunDef -> FunDef
optimizeFunDef (FunDef t f args ts) = FunDef t f args (optimizeTerminating ts)

optimizeTerminating :: Terminating -> Terminating
optimizeTerminating (Terminating nt r) = Terminating (optimizeNonTerminating nt) (optimizeReturn r)

optimizeNonTerminating :: NonTerminating -> NonTerminating
optimizeNonTerminating [] = []
optimizeNonTerminating ((If c a (Left b)) : ss) = If c a' (Left b') : ss'
  where
    ss' = optimizeNonTerminating ss
    a'  = optimizeNonTerminating a
    b'  = optimizeNonTerminating b
optimizeNonTerminating ((If c a (Right (flp, Terminating nt r))) : ss) =
  [If c a' (Right (flp, Terminating nt' r'))]
  where
    a'  = optimizeNonTerminating (a ++ ss)
    r'  = optimizeReturn r
    nt' = optimizeNonTerminating nt
optimizeNonTerminating (s:ss) = s:optimizeNonTerminating ss

optimizeReturn :: Return -> Return
optimizeReturn (IteReturn c a b) = IteReturn c (optimizeTerminating a) (optimizeTerminating b)
optimizeReturn r = r