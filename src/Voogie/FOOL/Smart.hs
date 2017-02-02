module Voogie.FOOL.Smart (
  module Voogie.FOOL.Smart,
  Var, Identifier, Definition(..), Binding(..), Term, Formula
) where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Data.Char
import Data.Either

import Voogie.Theory

import Voogie.FOOL
import Voogie.FOOL.Tuple

var :: String -> Var
var = Var . map toUpper

name :: String -> Name
name = map toLower

-- Definition
tupleD :: NonEmpty Identifier -> Definition
tupleD = either (flip Symbol []) TupleD . nonUnit

-- Term
integerConstant = IntegerConstant
booleanConstant = BooleanConstant

constant :: Identifier -> Term
constant = flip Application []

variable = Variable
application = Application
binary = Binary
unary = Unary
quantify = Quantify
equals = Equals

let_ :: Binding -> Term -> Term
let_ (Binding (Symbol c []) b) (Application c' []) | c == c' = b
let_ (Binding (TupleD t)    b) (TupleLiteral t') | fmap constant t == t' = b
let_ b t = Let b t

if_ = If
select = Select
store = Store

tupleLiteral :: NonEmpty Term -> Term
tupleLiteral = either id TupleLiteral . nonUnit