module Voogie.Boogie.Smart (
  module Voogie.Boogie.Smart,
  TopLevel, Statement, Assignment, Property, Expression, LValue, Main, Boogie
) where

import qualified Data.List.NonEmpty as NE (nonEmpty)
import Data.List.NonEmpty (NonEmpty)

import Voogie.Boogie
import Voogie.FOOL (Formula)

lvalue :: Var -> [NonEmpty Expression] -> LValue
lvalue = LValue

integerLiteral :: Integer -> Expression
integerLiteral = IntegerLiteral

booleanLiteral :: Bool -> Expression
booleanLiteral = BooleanLiteral

ref :: LValue -> Expression
ref = Ref

unary :: UnaryOp -> Expression -> Expression
unary = Unary

binary :: BinaryOp -> Expression -> Expression -> Expression
binary = Binary

ifElse :: Expression -> Expression -> Expression -> Expression
ifElse = IfElse

funApp :: Function -> NonEmpty Expression -> Expression
funApp = FunApp

equals :: Sign -> Expression -> Expression -> Expression
equals = Equals

assign :: NonEmpty (LValue, Expression) -> Maybe Statement
assign ass = Just (Assign ass)

if_ :: Expression -> [Statement] -> [Statement] -> Maybe Statement
if_ c a b
  | Just a' <- NE.nonEmpty a = Just (If c False a' b)
  | Just b' <- NE.nonEmpty b = Just (If c True b' [])
  | otherwise = Nothing

assume, assert :: Formula -> Property
assume = Assume
assert = Assert

main :: [Name] -> [Formula] -> [TopLevel] -> [Formula] -> Main
main = Main

boogie :: [Var] -> Main -> Boogie
boogie = Boogie
