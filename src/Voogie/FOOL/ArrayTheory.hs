module Voogie.FOOL.ArrayTheory (
  Instantiation,
  arrayTypeName, arrayType,
  selectSymbol, storeSymbol,
  theory
) where

import qualified Voogie.NonEmpty as VNE

import Voogie.Theory
import Voogie.FOOL
import Voogie.FOOL.Smart

import Voogie.BoogiePretty

type Instantiation = (Type, Type)

arrayTypeName :: Instantiation -> Name
arrayTypeName (tau, sigma) = "array_" ++ pretty tau ++ "_" ++ pretty sigma

arrayType :: Instantiation -> Type
arrayType = Custom . arrayTypeName

-- select : array(τ, σ) × τ → σ
selectSymbol :: Instantiation -> Identifier
selectSymbol i@(tau, sigma) = Typed t ("select_" ++ pretty tau ++ "_" ++ pretty sigma)
  where t = Functional (VNE.two (arrayType i) tau) sigma

-- store : array(τ, σ) × τ × σ → array(τ, σ)
storeSymbol :: Instantiation -> Identifier
storeSymbol i@(tau, sigma) = Typed t ("store_" ++ pretty tau ++ "_" ++ pretty sigma)
  where t = Functional (VNE.three (arrayType i) tau sigma) (arrayType i)

theory :: Instantiation -> Theory
theory t@(tau, sigma) =
  Theory [arrayTypeName t] [selectSymbol t, storeSymbol t]
         [readOverWrite1, readOverWrite2, extensionality]
  where
    -- (∀ a: array(τ, σ)) (∀ v: σ) (∀ i: τ) (∀ j: τ)
    --   (i = j => select(store(a, i, v), j) = v)
    readOverWrite1 = forall (VNE.four a' v' i' j')
                            (i === j ==> select (store a i v) j === v)

    -- (∀ a: array(τ, σ)) (∀ v: σ) (∀ i: τ) (∀ j: τ)
    --   (i != j => select(store(a, i, v), j) = select(a, j))
    readOverWrite2 = forall (VNE.four a' v' i' j')
                            (i =/= j ==> select (store a i v) j === select a j)

    -- (∀ a: array(τ, σ)) (∀ b: array(τ, σ))
    --   ((∀ i: τ) (select(a, i) = select(b, i)) => a = b)
    extensionality = forall (VNE.two a' b')
                            (forall (VNE.one i')
                                    (select a i === select a i) ==> a === b)

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

    select a i = application (selectSymbol t) [a, i]
    store a i v = application (storeSymbol t) [a, i, v]
