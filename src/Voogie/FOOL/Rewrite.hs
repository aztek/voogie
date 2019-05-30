{-# LANGUAGE RankNTypes #-}

module Voogie.FOOL.Rewrite where

import Control.Monad.Writer

import Voogie.Theory
import Voogie.FOOL

type Rewriter m a = a -> Maybe (Writer m a)

rewriteType :: Monoid m
            => Rewriter m Type
            -> Type -> Writer m Type
rewriteType typec t
  | Just c <- typec t = c
  | otherwise = traverseType (rewriteType typec) t

rewriteSymbol :: Monoid m
              => Rewriter m Type
              -> Typed a -> Writer m (Typed a)
rewriteSymbol typec i@(Typed t a)
  | Just c <- typec t = flip Typed a <$> c
  | otherwise = pure i

rewriteTerm :: Monoid m
            => Rewriter m Term
            -> Rewriter m Type
            -> Term -> Writer m Term
rewriteTerm termc typec t
  | Just c <- termc t = c
  | otherwise = traverseTerm (rewriteTerm termc typec) (rewriteSymbol typec) t

traverseType :: Monoid m
             => (Type -> Writer m Type)
             -> Type -> Writer m Type
traverseType f = \case
  Array      ts t -> Array      <$> traverse f ts <*> f t
  TupleType    ts -> TupleType  <$> traverse f ts
  Functional ts t -> Functional <$> traverse f ts <*> f t
  t               -> pure t

traverseTerm :: Monoid m
             => (Term -> Writer m Term)
             -> (forall a. Typed a -> Writer m (Typed a))
             -> Term -> Writer m Term
traverseTerm rt rs = \case
  Variable        v -> Variable     <$> rs v
  Constant        i -> Constant     <$> rs i
  Application  f ts -> Application  <$> rs f <*> traverse rt ts
  Binary     op f g -> Binary op    <$> rt f <*> rt g
  Unary        op f -> Unary op     <$> rt f
  Quantify   q vs f -> Quantify q   <$> traverse rs vs <*> rt f
  Equals      s a b -> Equals s     <$> rt a <*> rt b
  Let           b t -> Let          <$> traverseBinding rt rs b <*> rt t
  If          c a b -> If           <$> rt c <*> rt a <*> rt b
  Select        a i -> Select       <$> rt a <*> rt i
  Store       a i v -> Store        <$> rt a <*> rt i <*> rt v
  TupleLiteral   es -> TupleLiteral <$> traverse rt es
  t                 -> pure t

traverseBinding :: Monoid m
                => (Term -> Writer m Term)
                -> (forall a. Typed a -> Writer m (Typed a))
                -> Binding -> Writer m Binding
traverseBinding rt rs = \case
  Binding d b -> Binding <$> traverseDefinition rs d <*> rt b

traverseDefinition :: Monoid m
                   => (forall a. Typed a -> Writer m (Typed a))
                   -> Definition -> Writer m Definition
traverseDefinition rs = \case
  ConstantSymbol i -> ConstantSymbol <$> rs i
  Function    i vs -> Function       <$> rs i <*> traverse rs vs
  TupleD        is -> TupleD         <$> traverse rs is
