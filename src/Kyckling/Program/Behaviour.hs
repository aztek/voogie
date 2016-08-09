module Kyckling.Program.Behaviour where

import Control.Applicative

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import qualified Data.Set as S
import Data.Set (Set, (\\), union)

import Kyckling.Theory
import Kyckling.Program

data Behaviour = Behaviour { returns :: Maybe Type, declares :: Set Var, updates :: Set Var }

initBehaviour :: Behaviour
initBehaviour = Behaviour Nothing S.empty S.empty

updated :: Behaviour -> Set Var
updated (Behaviour _ d u) = u \\ d


getBehaviour :: Scoped Statement -> Behaviour
getBehaviour (Scoped decls (Assign ass)) = Behaviour Nothing (S.fromList decls) (S.fromList $ NE.toList $ fmap (lvariable . fst) ass)
getBehaviour (Scoped decls (If c a b)) = behaviour { declares = S.fromList decls }
  where
    behaviour = mergeBehaviours a' b'
    a' = getBehaviourNonTerminating a
    b' = either getBehaviourNonTerminating (getBehaviourTerminating . snd) b


getBehaviourReturn :: Scoped Return -> Behaviour
getBehaviourReturn (Scoped decls (Return e)) = Behaviour (Just $ typeOf e) (S.fromList decls) S.empty
getBehaviourReturn (Scoped decls (IteReturn _ a b)) = behaviour { declares = S.fromList decls }
  where
    behaviour = mergeBehaviours a' b'
    a' = getBehaviourTerminating a
    b' = getBehaviourTerminating b

getBehaviourTerminating :: Terminating -> Behaviour
getBehaviourTerminating (Terminating ss r) = mergeBehaviours (getBehaviourNonTerminating ss) (getBehaviourReturn r)


mergeBehaviours :: Behaviour -> Behaviour -> Behaviour
mergeBehaviours (Behaviour r1 d1 u1) (Behaviour r2 d2 u2) = Behaviour (r1 <|> r2) (d1 `union` d2) (u1 `union` u2)

getBehaviourNonTerminating :: NonTerminating -> Behaviour
getBehaviourNonTerminating = foldr (mergeBehaviours . getBehaviour) initBehaviour