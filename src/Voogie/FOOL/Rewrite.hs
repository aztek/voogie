module Voogie.FOOL.Rewrite where

import Voogie.Theory
import Voogie.FOOL

rewriteType :: Applicative f
            => (Type -> Maybe (f Type))
            -> Type -> f Type
rewriteType typeF t
  | Just c <- typeF t = c
  | otherwise = traverseType (rewriteType typeF) t

rewriteTerm :: Applicative f
            => (Term -> Maybe (f Term))
            -> (Type -> Maybe (f Type))
            -> Term -> f Term
rewriteTerm termF typeF t
  | Just c <- termF t = c
  | otherwise = traverseTerm (rewriteTerm termF typeF) (rewriteType typeF) t

traverseProblem :: Applicative f
                => (Term -> f Term)
                -> Problem -> f Problem
traverseProblem termF (Problem types symbols axioms conjecture) =
  Problem types symbols <$> traverse termF axioms <*> termF conjecture

traverseType :: Applicative f
             => (Type -> f Type)
             -> Type -> f Type
traverseType typeF = \case
  Array      ts t -> Array      <$> traverse typeF ts <*> typeF t
  TupleType    ts -> TupleType  <$> traverse typeF ts
  Functional ts t -> Functional <$> traverse typeF ts <*> typeF t
  t               -> pure t

traverseTyped :: Applicative f
              => (Type -> f Type)
              -> Typed a -> f (Typed a)
traverseTyped typeF (Typed t a) = Typed <$> typeF t <*> pure a

traverseTerm :: Applicative f
             => (Term -> f Term)
             -> (Type -> f Type)
             -> Term -> f Term
traverseTerm termF typeF = \case
  Variable       v -> Variable     <$> traverseTyped typeF v
  Constant       i -> Constant     <$> traverseTyped typeF i
  Application f ts -> Application  <$> traverseTyped typeF f <*> traverse termF ts
  Binary    op f g -> Binary op    <$> termF f <*> termF g
  Unary       op f -> Unary op     <$> termF f
  Quantify  q vs f -> Quantify q   <$> traverse (traverseTyped typeF) vs <*> termF f
  Equals     s a b -> Equals s     <$> termF a <*> termF b
  Let          b t -> Let          <$> traverseBinding termF typeF b <*> termF t
  If         c a b -> If           <$> termF c <*> termF a <*> termF b
  Select       a i -> Select       <$> termF a <*> termF i
  Store      a i v -> Store        <$> termF a <*> termF i <*> termF v
  TupleLiteral  es -> TupleLiteral <$> traverse termF es
  t                -> pure t

traverseBinding :: Applicative f
                => (Term -> f Term)
                -> (Type -> f Type)
                -> Binding -> f Binding
traverseBinding termF typeF (Binding d b) =
  Binding <$> traverseDefinition typeF d <*> termF b

traverseDefinition :: Applicative f
                   => (Type -> f Type)
                   -> Definition -> f Definition
traverseDefinition typeF = \case
  ConstantSymbol i -> ConstantSymbol <$> traverseSymbol i
  Function    i vs -> Function       <$> traverseSymbol i <*> traverse traverseSymbol vs
  TupleD        is -> TupleD         <$> traverse traverseSymbol is
  where
    traverseSymbol = traverseTyped typeF
