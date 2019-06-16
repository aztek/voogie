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

import qualified Data.List.NonEmpty as NE (one, two, three, four)

import Voogie.FOOL
import Voogie.FOOL.Smart (application, variable, var, forall, (===), (=/=), (==>))
import Voogie.Pretty (pretty, displayS, renderCompact)

type Instantiation = (Type, Type)

instantiationName :: Instantiation -> Name
instantiationName (tau, sigma) = printType tau ++ printType sigma
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
storeSymbol inst = Typed (arrayType i) ("store" ++ instantiationName inst)

select :: Instantiation -> Term -> Term -> Term
select inst arr index = application (selectSymbol inst) (NE.two arr index)

store :: Instantiation -> Term -> Term -> Term -> Term
store inst arr index val = application (storeSymbol inst) (NE.three arr index val)

theory :: Instantiation -> Theory
theory inst@(tau, sigma) =
  Theory [arrayTypeName inst] [selectSymbol inst, storeSymbol inst]
         [readOverWrite1, readOverWrite2, extensionality]
  where
    -- (∀ a: array(τ, σ)) (∀ v: σ) (∀ i: τ) (∀ j: τ)
    --   (i = j => select(store(a, i, v), j) = v)
    readOverWrite1 = forall (NE.four a' v' i' j')
                            (i === j ==> select inst (store inst a i v) j === v)

    -- (∀ a: array(τ, σ)) (∀ v: σ) (∀ i: τ) (∀ j: τ)
    --   (i != j => select(store(a, i, v), j) = select(a, j))
    readOverWrite2 = forall (NE.four a' v' i' j')
                            (i =/= j ==> select inst (store inst a i v) j === select inst a j)

    -- (∀ a: array(τ, σ)) (∀ b: array(τ, σ))
    --   ((∀ i: τ) (select(a, i) = select(b, i)) => a = b)
    extensionality = forall (NE.two a' b')
                            (forall (NE.one i')
                                    (select inst a i === select inst b i) ==> a === b)

    a' = Typed (arrayType t) (var "a")
    b' = Typed (arrayType t) (var "b")
    v' = Typed sigma (var "v")
    i' = Typed tau (var "i")
    j' = Typed tau (var "j")

    a = variable a'
    b = variable b'
    v = variable v'
    i = variable i'
    j = variable j'
