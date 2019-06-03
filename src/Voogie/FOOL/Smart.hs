module Voogie.FOOL.Smart (
  module Voogie.FOOL.Smart,
  Var, VarList, Identifier, Definition(..), Binding(..), Term(..), Formula,
  Problem(..), appendTheory, appendTheories
) where

import qualified Data.List.NonEmpty as NE (toList)
import Data.List.NonEmpty (NonEmpty)

import Voogie.FOOL
import Voogie.FOOL.Tuple (nonUnit)
import Voogie.FOOL.TypeSafe
import Voogie.Theory

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
application = typeSafeApplication <$> fmap name

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

conjunction :: [Formula] -> Formula
conjunction = getConjunction . mconcat . fmap Conjunction

let_ :: Binding -> Term -> Term
let_ b@(Binding d s) t
  | trivialDefinition d t = s
  | otherwise = Let b t

trivialDefinition :: Definition -> Term -> Bool
trivialDefinition (ConstantSymbol c) (Constant c') = c == c'
trivialDefinition (TupleD t) (TupleLiteral t') = fmap constant t == t'
trivialDefinition _ _ = False

if_ :: Term -> Term -> Term -> Term
if_ = If

select :: Foldable f => Term -> f Term -> Term
select = foldl typeSafeSelect

store :: Term -> NonEmpty Term -> Term -> Term
store a = store' . reverse . NE.toList
  where
   store' :: [Term] -> Term -> Term
   store' []     t = t
   store' (i:is) t = store' is $ typeSafeStore (select a $ reverse is) i t

tupleLiteral :: NonEmpty Term -> Term
tupleLiteral = either id TupleLiteral . nonUnit
