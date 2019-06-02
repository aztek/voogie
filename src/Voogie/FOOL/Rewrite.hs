{-# LANGUAGE LambdaCase #-}

module Voogie.FOOL.Rewrite where

import Voogie.Theory
import Voogie.FOOL

traverseProblem :: Applicative f
                => (Term -> f Term)
                -> (Type -> f Type)
                -> Problem -> f Problem
traverseProblem termF typeF (Problem ts ss as c) =
  Problem ts <$> traverse (traverseTyped typeF) ss
             <*> traverse termF as
             <*> termF c

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
  Variable       v -> Variable     <$> typedF v
  Constant       i -> Constant     <$> typedF i
  Application f ts -> Application  <$> typedF f <*> traverse termF ts
  Binary    op f g -> Binary op    <$> termF f <*> termF g
  Unary       op f -> Unary op     <$> termF f
  Quantify  q vs f -> Quantify q   <$> traverse typedF vs <*> termF f
  Equals     s a b -> Equals s     <$> termF a <*> termF b
  Let          b t -> Let          <$> traverseBinding termF typeF b <*> termF t
  If         c a b -> If           <$> termF c <*> termF a <*> termF b
  Select       a i -> Select       <$> termF a <*> termF i
  Store      a i v -> Store        <$> termF a <*> termF i <*> termF v
  TupleLiteral  es -> TupleLiteral <$> traverse termF es
  t                -> pure t
  where
    typedF = traverseTyped typeF

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
  ConstantSymbol i -> ConstantSymbol <$> typedF i
  Function    i vs -> Function       <$> typedF i <*> traverse typedF vs
  TupleD        is -> TupleD         <$> traverse typedF is
  where
    typedF = traverseTyped typeF
