module Kyckling.Program.Types where

import Kyckling.Program

unaryOpTypes :: UnaryOp -> (Type, Type)
unaryOpTypes op =
  case op of
    Negate   -> (Boolean, Boolean)
    Positive -> (Integer, Integer)
    Negative -> (Integer, Integer)

unaryOpDomain, unaryOpRange :: UnaryOp -> Type
unaryOpDomain = fst . unaryOpTypes
unaryOpRange  = snd . unaryOpTypes


binaryOpTypes :: BinaryOp -> ((Type, Type), Type)
binaryOpTypes op =
  case op of
    And      -> ((Boolean, Boolean), Boolean)
    Or       -> ((Boolean, Boolean), Boolean)
    Greater  -> ((Integer, Integer), Boolean)
    Less     -> ((Integer, Integer), Boolean)
    Geq      -> ((Integer, Integer), Boolean)
    Leq      -> ((Integer, Integer), Boolean)
    Add      -> ((Integer, Integer), Integer)
    Subtract -> ((Integer, Integer), Integer)
    Multiply -> ((Integer, Integer), Integer)

binaryOpDomain :: BinaryOp -> (Type, Type)
binaryOpDomain = fst . binaryOpTypes

binaryOpRange :: BinaryOp -> Type
binaryOpRange = snd . binaryOpTypes