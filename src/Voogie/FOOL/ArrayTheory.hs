{-# LANGUAGE CPP #-}

{-|
Module       : Voogie.FOOL.ArrayTheory
Description  : The theory of arrays embedded into FOOL.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.FOOL.ArrayTheory (
  Instantiation,
  arrayType,
  select,
  store,
  theory
) where

import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE (zipWith)
import Data.List.NonEmpty.Extra (snoc)
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup(..))
#endif

import Voogie.FOOL
import Voogie.FOOL.Smart (application, variable, var, forall, conjunction,
                          (===), (=/=), (==>))
import Voogie.Pretty (pretty, displayS, renderCompact)

type Instantiation = (NonEmpty Type, Type)

instantiationName :: Instantiation -> Name
instantiationName (taus, sigma) = concatMap printType taus ++ printType sigma
  where
    printType t = showChar '_' . displayS (renderCompact $ pretty t) $ ""

arrayTypeName :: Instantiation -> Name
arrayTypeName inst = "array" ++ instantiationName inst

arrayType :: Instantiation -> Type
arrayType = Custom . arrayTypeName

-- select : array(τ, σ) × τ → σ
selectSymbol :: Instantiation -> Identifier
selectSymbol inst@(_, sigma) = Typed sigma ("select" ++ instantiationName inst)

-- store : array(τ, σ) × τ × σ → array(τ, σ)
storeSymbol :: Instantiation -> Identifier
storeSymbol inst = Typed (arrayType inst) ("store" ++ instantiationName inst)

select :: Instantiation -> Term -> NonEmpty Term -> Term
select inst a i = application (selectSymbol inst) (a <| i)

store :: Instantiation -> Term -> NonEmpty Term -> Term -> Term
store inst a i e = application (storeSymbol inst) (snoc (a <| i) e)

theory :: Instantiation -> Theory
theory inst@(taus, sigma) =
  Theory [arrayTypeName inst] [selectSymbol inst, storeSymbol inst]
         [readOverWrite1, readOverWrite2, extensionality]
  where
    -- (∀ a: array(τ, σ)) (∀ v: σ) (∀ I: T) (∀ J: T)
    --   (I = J => select(store(a, I, v), J) = v)
    readOverWrite1 = forall (a' <| v' <| is' <> js') $
      conjunction (NE.zipWith (===) is js)
        ==>
      select inst (store inst a is v) js === v

    -- (∀ a: array(τ, σ)) (∀ v: σ) (∀ I: T) (∀ J: T)
    --   (I != J => select(store(a, I, v), J) = select(a, J))
    readOverWrite2 = forall (a' <| v' <| is' <> js') $
      conjunction (NE.zipWith (=/=) is js)
        ==>
      select inst (store inst a is v) js === select inst a js

    -- (∀ a: array(τ, σ)) (∀ b: array(τ, σ))
    --   ((∀ i: τ) (select(a, i) = select(b, i)) => a = b)
    extensionality = forall (a' <| b' <| is') $
      select inst a is === select inst b is
        ==>
      a === b

    a' = Typed (arrayType inst) (var "a")
    b' = Typed (arrayType inst) (var "b")
    v' = Typed sigma (var "v")
    is' = NE.zipWith Typed taus (vars "i")
    js' = NE.zipWith Typed taus (vars "j")

    vars p = fmap (var . \n -> p <> show n) (0 :| [(1::Int)..])

    a = variable a'
    b = variable b'
    v = variable v'
    is = fmap variable is'
    js = fmap variable js'
