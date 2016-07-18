module Kyckling.FOOL.Smart (
  module Kyckling.FOOL.Smart,
  Var(..), Identifier, Definition(..), Binding(..), Term, Formula
) where

import Kyckling.FOOL

constant :: Identifier -> Term
constant = flip Application []

integerConstant = IntegerConstant
booleanConstant = BooleanConstant
variable = Variable
application = Application
binary = Binary
unary = Unary
quantify = Quantify
equals = Equals
let_ = Let
if_ = If
select = Select
store = Store
tupleLiteral = TupleLiteral
nothing = Nothing_
just = Just_
isJust = IsJust
fromJust = FromJust
left = Left_
right = Right_
isLeft = IsLeft
fromLeft = FromLeft
fromRight = FromRight