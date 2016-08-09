module Kyckling.Program.Smart (
  module Kyckling.Program.Smart,
  module Kyckling.Program
) where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Data.Either

import Kyckling.Program

assign :: NonEmpty (LValue, Expression) -> Scoped Statement
assign = Scoped [] . Assign

if_ :: Expression -> NonTerminating -> Either NonTerminating (Bool, Terminating) -> Scoped Statement
if_ c a b = Scoped [] (If c a b)

return_ :: Expression -> Scoped Return
return_ = Scoped [] . Return

iteReturn :: Expression -> Terminating -> Terminating -> Scoped Return
iteReturn c a b = Scoped [] (IteReturn c a b) 