module Voogie.FOOL.Rewrite where

import Control.Monad.Writer

import Voogie.Theory
import Voogie.FOOL

type Rewriter a t = t -> Maybe (Writer a t)

rewriteType :: Monoid m => Rewriter m Type -> Type -> Writer m Type
rewriteType typec t
  | Just c <- typec t = c
  | otherwise = inlineType t
  where
    inline = rewriteType typec

    inlineType = \case
      Array      ts t -> Array      <$> traverse inline ts <*> inline t
      TupleType    ts -> TupleType  <$> traverse inline ts
      Functional ts t -> Functional <$> traverse inline ts <*> inline t
      t               -> pure t

rewriteSymbol :: Monoid m => Rewriter m Type -> Typed a -> Writer m (Typed a)
rewriteSymbol typec i@(Typed t a)
  | Just c <- typec t = flip Typed a <$> c
  | otherwise = pure i

rewriteTerm :: Monoid m =>
               Rewriter m Term -> Rewriter m Type -> Term -> Writer m Term
rewriteTerm termc typec t
  | Just c <- termc t = c
  | otherwise = inlineTerm t
  where
    inline = rewriteTerm termc typec

    inlineTerm = \case
      Variable        v -> Variable     <$> rewriteSymbol typec v
      Constant        i -> Constant     <$> rewriteSymbol typec i
      Application  f ts -> Application  <$> rewriteSymbol typec f <*> traverse inline ts
      Binary     op f g -> Binary op    <$> inline f <*> inline g
      Unary        op f -> Unary op     <$> inline f
      Quantify   q vs f -> Quantify q   <$> traverse (rewriteSymbol typec) vs <*> inline f
      Equals      s a b -> Equals s     <$> inline a <*> inline b
      Let           b t -> Let          <$> inlineBinding b <*> inline t
      If          c a b -> If           <$> inline c <*> inline a <*> inline b
      Select        a i -> Select       <$> inline a <*> inline i
      Store       a i v -> Store        <$> inline a <*> inline i <*> inline v
      TupleLiteral   es -> TupleLiteral <$> traverse inline es
      t                 -> pure t

    inlineBinding (Binding d b) = Binding <$> inlineDefinition d <*> inline b

    inlineDefinition = \case
      ConstantSymbol i -> ConstantSymbol <$> rewriteSymbol typec i
      Function    i vs -> Function       <$> rewriteSymbol typec i <*> traverse (rewriteSymbol typec) vs
      TupleD        is -> TupleD         <$> traverse (rewriteSymbol typec) is
