module Kyckling.Theory where

data Type = Boolean
          | Integer
          | Array Type
  deriving (Show, Eq)

type Signature = [(String, Type)]