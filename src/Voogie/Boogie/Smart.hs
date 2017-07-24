module Voogie.Boogie.Smart (
  module Voogie.Boogie.Smart,
  Statement, Assume, Expression, LValue
) where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

import Data.Char

import Voogie.Theory
import Voogie.Boogie

lvalue = LValue

integerLiteral = IntegerLiteral
booleanLiteral = BooleanLiteral
ref = Ref
unary = Unary
binary = Binary
ifElse = IfElse
funApp = FunApp
equals = Equals

assign :: NonEmpty (LValue, Expression) -> Maybe Statement
assign ass = Just (Assign ass)

if_ :: Expression -> [Statement] -> [Statement] -> Maybe Statement
if_ c a b = case NE.nonEmpty a of
                Just a' -> Just (If c False a' b)
                Nothing -> case NE.nonEmpty b of
                             Nothing -> Nothing
                             Just b' -> Just (If c True b' [])

assume = Assume

main = Main

boogie = Boogie
