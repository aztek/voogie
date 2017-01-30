module Voogie.Boogie.Optimize where

import Voogie.Boogie

optimize :: Boogie -> Boogie
optimize (Boogie fs ss as) = Boogie (map optimizeFunDef fs) (optimizeNonTerminating ss) as

optimizeFunDef :: FunDef -> FunDef
optimizeFunDef (FunDef f args ts) = FunDef f args (optimizeTerminating ts)

optimizeTerminating :: Terminating -> Terminating
optimizeTerminating (Terminating nt r) = Terminating (optimizeNonTerminating nt) (fmap optimizeReturn r)

optimizeNonTerminating :: NonTerminating -> NonTerminating
optimizeNonTerminating [] = []
optimizeNonTerminating (Scoped scope (If c a (Left b)) : ss) = Scoped scope (If c a' (Left b')) : ss'
  where
    ss' = optimizeNonTerminating ss
    a'  = optimizeNonTerminating a
    b'  = optimizeNonTerminating b
optimizeNonTerminating (Scoped scope (If c a (Right (flp, Terminating nt r))) : ss) =
  [Scoped scope (If c a' (Right (flp, Terminating nt' r')))]
  where
    a'  = optimizeNonTerminating (a ++ ss)
    r'  = fmap optimizeReturn r
    nt' = optimizeNonTerminating nt
optimizeNonTerminating (s:ss) = s:optimizeNonTerminating ss

optimizeReturn :: Return -> Return
optimizeReturn (IteReturn c a b) = IteReturn c (optimizeTerminating a) (optimizeTerminating b)
optimizeReturn r = r
