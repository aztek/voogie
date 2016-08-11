module Kyckling.FOOL.Smart (
  module Kyckling.FOOL.Smart,
  Var(..), Identifier, Definition(..), Binding(..), Term, Formula
) where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Data.Either

import Kyckling.Theory

import Kyckling.FOOL
import Kyckling.FOOL.Tuple

-- Binding
binding :: NonEmpty (Identifier, Term) -> Binding
binding ass = Binding (tupleD ids) (tupleLiteral bodies)
  where
    (ids, bodies) = NE.unzip ass

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
let_ (Binding (Symbol c []) b) (Right_ t (Application c' [])) | c == c' = Right_ t b
let_ (Binding (Symbol c []) b) (Left_  (Application c' []) t) | c == c' = Left_  b t
let_ (Binding (Symbol c []) b) (Some  (Application c' []))   | c == c' = Some  b
let_ b t = Let b t

if_ = If
select = Select
store = Store

tupleLiteral :: NonEmpty Term -> Term
tupleLiteral = either id TupleLiteral . nonUnit

noen = None
some = Some
isSome = IsSome
fromSome = FromSome

left :: Term -> Type -> Term
left (FromLeft t) _ = t
left a t = Left_ a t

right :: Type -> Term -> Term
right _ (FromRight t) = t
right t b = Right_ t b

isLeft = IsLeft

fromLeft :: Term -> Term
fromLeft (Left_ a _) = a
fromLeft t = FromLeft t

fromRight :: Term -> Term
fromRight (Right_ _ a) = a
fromRight t = FromRight t