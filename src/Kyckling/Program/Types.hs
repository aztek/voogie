module Kyckling.Program.Types where

import Kyckling.Program

unaryOpDomain :: UnaryOp -> Type
unaryOpDomain op = 
  case op of
    Negate   -> Boolean
    Positive -> Integer
    Negative -> Integer

unaryOpRange :: UnaryOp -> Type
unaryOpRange op = 
  case op of
    Negate   -> Boolean
    Positive -> Integer
    Negative -> Integer

binaryOpDomain :: BinaryOp -> (Type, Type)
binaryOpDomain op =
  case op of
    And      -> (Boolean, Boolean)
    Or       -> (Boolean, Boolean)
    Greater  -> (Integer, Integer)
    Less     -> (Integer, Integer)
    Geq      -> (Integer, Integer)
    Leq      -> (Integer, Integer)
    Add      -> (Integer, Integer)
    Subtract -> (Integer, Integer)
    Multiply -> (Integer, Integer)

binaryOpRange :: BinaryOp -> Type
binaryOpRange op =
  case op of
    And      -> Boolean
    Or       -> Boolean
    Greater  -> Boolean
    Less     -> Boolean
    Geq      -> Boolean
    Leq      -> Boolean
    Add      -> Integer
    Subtract -> Integer
    Multiply -> Integer