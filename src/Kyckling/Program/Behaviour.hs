module Kyckling.Program.Behaviour (
  Behaviour(..), 
  getBehaviour, initBehaviour, getBehaviourTerminating, getBehaviourNonTerminating, collectDeclarations
) where

import Control.Applicative

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import qualified Data.Set as S
import Data.Set (Set, (\\), union)

import Kyckling.Theory
import Kyckling.Program

data Behaviour = Behaviour { returns :: Maybe Type, updates :: Set Var }
  deriving (Show)

initBehaviour :: Behaviour
initBehaviour = Behaviour Nothing S.empty

getBehaviour :: Scoped Statement -> Behaviour
getBehaviour (Scoped _ (Assign ass)) = Behaviour Nothing (S.fromList $ NE.toList $ fmap (lvariable . fst) ass)
getBehaviour (Scoped _ (If _ a b))   = mergeBehaviours a' b'
  where
    a' = getBehaviourNonTerminating a
    b' = either getBehaviourNonTerminating (getBehaviourTerminating . snd) b

getBehaviourReturn :: Scoped Return -> Behaviour
getBehaviourReturn (Scoped _ (Return e)) = Behaviour (Just $ typeOf e) S.empty
getBehaviourReturn (Scoped _ (IteReturn _ a b)) = mergeBehaviours (getBehaviourTerminating a) (getBehaviourTerminating b)

getBehaviourTerminating :: Terminating -> Behaviour
getBehaviourTerminating (Terminating ss r) = Behaviour r' (u' \\ collectDeclarations ss)
  where
    Behaviour r' u' = mergeBehaviours (getBehaviourNonTerminating ss) (getBehaviourReturn r)

mergeBehaviours :: Behaviour -> Behaviour -> Behaviour
mergeBehaviours (Behaviour r1 u1) (Behaviour r2 u2) = Behaviour (r1 <|> r2) (u1 `union` u2)

getBehaviourNonTerminating :: NonTerminating -> Behaviour
getBehaviourNonTerminating nt = Behaviour r (u \\ collectDeclarations nt)
  where
    Behaviour r u = foldr (mergeBehaviours . getBehaviour) initBehaviour nt

collectDeclarations :: NonTerminating -> Set Var
collectDeclarations = S.fromList . concatMap (\(Scoped s _) -> s)