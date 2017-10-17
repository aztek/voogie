module Voogie.FOOL.Smart (
  module Voogie.FOOL.Smart,
  Var, Identifier, Definition(..), Binding(..), Term, Formula, Conjunction(..)
) where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Voogie.Theory

import Voogie.FOOL
import Voogie.FOOL.Tuple (nonUnit)
import Voogie.FOOL.TypeSafe

var :: String -> Var
var = Var

name :: String -> Name
name = id

-- Definition
tupleD :: NonEmpty Identifier -> Definition
tupleD = either ConstantSymbol TupleD . nonUnit

-- Term
integerConstant = IntegerConstant
booleanConstant = BooleanConstant

constant :: Identifier -> Term
constant = Constant . fmap name

variable = Variable

application :: Identifier -> [Term] -> Term
application i = maybe (constant i) (Application $ name <$> i) . NE.nonEmpty

binary = Binary
unary = Unary
quantify = Quantify
equals = Equals

let_ :: Binding -> Term -> Term
let_ (Binding (ConstantSymbol c) b) (Constant c')     | c == c'               = b
let_ (Binding (TupleD t)         b) (TupleLiteral t') | fmap constant t == t' = b
let_ b t = Let b t

if_ = If

select :: Foldable f => Term -> f Term -> Term
select = foldl typeSafeSelect

store :: Term -> NonEmpty Term -> Term -> Term
store a = store' . reverse . NE.toList
  where
   store' :: [Term] -> Term -> Term
   store' []     t = t
   store' (i:is) t = store' is (typeSafeStore (select a (reverse is)) i t)

tupleLiteral :: NonEmpty Term -> Term
tupleLiteral = either id TupleLiteral . nonUnit
