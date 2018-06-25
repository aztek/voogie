module Voogie.FOOL.Rewrite where

import Voogie.Theory
import Voogie.FOOL

newtype Accumulate m t = Accumulate (t, m)
  deriving (Show, Eq, Functor)

instance Monoid m => Applicative (Accumulate m) where
  pure t = Accumulate (t, mempty)
  Accumulate (f, m1) <*> Accumulate (a, m2) = Accumulate (f a, m1 `mappend` m2)

type Rewriter a t = t -> Maybe (Accumulate a t)

rewriteType :: Monoid m => Rewriter m Type -> Type -> Accumulate m Type
rewriteType typec t
  | Just c <- typec t = c
  | otherwise = case t of
    Array ts t -> Array <$> traverse inline ts <*> inline t
    TupleType ts -> TupleType <$> traverse inline ts
    Functional ts t -> Functional <$> traverse inline ts <*> inline t
    _ -> pure t
  where
    inline = rewriteType typec

rewriteSymbol :: Monoid m => Rewriter m Type -> Typed a -> Accumulate m (Typed a)
rewriteSymbol typec i@(Typed t a)
  | Just c <- typec t = flip Typed a <$> c
  | otherwise = pure i

rewriteTerm :: Monoid m =>
               Rewriter m Term -> Rewriter m Type -> Term -> Accumulate m Term
rewriteTerm termc typec t
  | Just c <- termc t = c
  | otherwise = case t of
    IntegerConstant{} -> pure t
    BooleanConstant{} -> pure t
    Variable v -> Variable <$> rewriteSymbol typec v
    Constant i -> Constant <$> rewriteSymbol typec i
    Application f ts -> Application <$> rewriteSymbol typec f <*> traverse inline ts
    Binary op f g -> Binary op <$> inline f <*> inline g
    Unary op f -> Unary op <$> inline f
    Quantify q vs f -> Quantify q <$> traverse (rewriteSymbol typec) vs <*> inline f
    Equals s a b -> Equals s <$> inline a <*> inline b
    Let (Binding d b) t -> Let <$> b' <*> inline t
      where
        b' = Binding <$> inlineDefinition d <*> inline b
    If c a b -> If <$> inline c <*> inline a <*> inline b
    Select a i -> Select <$> inline a <*> inline i
    Store a i v -> Store <$> inline a <*> inline i <*> inline v
    TupleLiteral es -> TupleLiteral <$> traverse inline es
  where
    inline = rewriteTerm termc typec

    inlineDefinition (ConstantSymbol i) =
      ConstantSymbol <$> rewriteSymbol typec i
    inlineDefinition (Function i vs) =
      Function <$> rewriteSymbol typec i <*> traverse (rewriteSymbol typec) vs
    inlineDefinition (TupleD is) =
      TupleD <$> traverse (rewriteSymbol typec) is
