{-|
Module       : Voogie.FOOL.Smart
Description  : Smart constructors for the FOOL data types.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.FOOL.Smart (
  Var,
  VarList,
  Identifier,
  Definition(..),
  Binding(..),
  Term(..),
  Formula,
  Problem(..),
  appendTheory,
  appendTheories,
  var,
  name,
  tupleD,
  integerConstant,
  booleanConstant,
  constant,
  variable,
  application,
  binary,
  (==>),
  unary,
  quantify,
  forall,
  exists,
  equals,
  (===),
  (=/=),
  conjunction,
  let_,
  ifElse,
  select,
  store,
  foldStore,
  tupleLiteral
) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonUnit (nonUnit)

import Voogie.FOOL
import Voogie.Pretty.Boogie.FOOL (pretty, displayS, renderCompact)

var :: Name -> Var
var = Var

name :: Name -> Name
name = id

-- Definition
tupleD :: NonEmpty Identifier -> Definition
tupleD = either ConstantSymbol TupleD . nonUnit

-- Term
integerConstant :: Integer -> Term
integerConstant = IntegerConstant

booleanConstant :: Bool -> Term
booleanConstant = BooleanConstant

constant :: Identifier -> Term
constant = Constant . fmap name

variable :: Typed Var -> Term
variable = Variable

application :: Identifier -> NonEmpty Term -> Term
application = Application

binary :: BinaryOp -> Term -> Term -> Term
binary = Binary

infixl 5 ==>
(==>) :: Formula -> Formula -> Formula
f ==> g = binary Imply f g

unary :: UnaryOp -> Term -> Term
unary = Unary

quantify :: Quantifier -> VarList -> Term -> Term
quantify = Quantify

forall :: VarList -> Term -> Term
forall = quantify Forall

exists :: VarList -> Term -> Term
exists = quantify Exists

equals :: Sign -> Term -> Term -> Term
equals = Equals

infixl 6 ===
(===) :: Term -> Term -> Term
x === y = equals Pos x y

infixl 6 =/=
(=/=) :: Term -> Term -> Term
x =/= y = equals Neg x y

conjunction :: Foldable t => t Formula -> Formula
conjunction = getConjunction . mconcat . fmap Conjunction . toList

let_ :: Binding -> Term -> Term
let_ b@(Binding d s) t
  | trivialDefinition d t = s
  | otherwise = Let b t

trivialDefinition :: Definition -> Term -> Bool
trivialDefinition (ConstantSymbol c) (Constant c') = c == c'
trivialDefinition (TupleD t) (TupleLiteral t') = fmap constant t == t'
trivialDefinition _ _ = False

ifElse :: Term -> Term -> Term -> Term
ifElse = IfElse

select :: Term -> NonEmpty Term -> Term
select arr index
  | Array i _ <- typeOf arr
  , fmap typeOf index == i = selectTerm
  | otherwise = error $ displayS (renderCompact $ pretty selectTerm)
                                 "ill-typed expression "
  where
    selectTerm = Select arr index

store :: Term -> NonEmpty Term -> Term -> Term
store arr index element
  | a@(Array i _) <- typeOf arr
  , fmap typeOf index == i
  , typeOf element == arrayArgument a = storeTerm
  | otherwise = error $ displayS (renderCompact $ pretty storeTerm)
                                 "ill-typed expression "
  where
    storeTerm = Store arr index element

tupleLiteral :: NonEmpty Term -> Term
tupleLiteral = either id TupleLiteral . nonUnit
