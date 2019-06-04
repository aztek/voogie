module Voogie.FOOL.ArrayTheory (
  Instantiation,
  arrayTypeName, arrayType,
  selectSymbol, storeSymbol,
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
arrayTypeName i = "array" ++ instantiationName i

arrayType :: Instantiation -> Type
arrayType = Custom . arrayTypeName

-- select : array(τ, σ) × τ → σ
selectSymbol :: Instantiation -> Identifier
selectSymbol i@(tau, sigma) = Typed t ("select" ++ instantiationName i)
  where t = Functional (NE.two (arrayType i) tau) sigma

-- store : array(τ, σ) × τ × σ → array(τ, σ)
storeSymbol :: Instantiation -> Identifier
storeSymbol i@(tau, sigma) = Typed t ("store" ++ instantiationName i)
  where t = Functional (NE.three (arrayType i) tau sigma) (arrayType i)

theory :: Instantiation -> Theory
theory t@(tau, sigma) =
  Theory [arrayTypeName t] [selectSymbol t, storeSymbol t]
         [readOverWrite1, readOverWrite2, extensionality]
  where
    -- (∀ a: array(τ, σ)) (∀ v: σ) (∀ i: τ) (∀ j: τ)
    --   (i = j => select(store(a, i, v), j) = v)
    readOverWrite1 = forall (NE.four a' v' i' j')
                            (i === j ==> select (store a i v) j === v)

    -- (∀ a: array(τ, σ)) (∀ v: σ) (∀ i: τ) (∀ j: τ)
    --   (i != j => select(store(a, i, v), j) = select(a, j))
    readOverWrite2 = forall (NE.four a' v' i' j')
                            (i =/= j ==> select (store a i v) j === select a j)

    -- (∀ a: array(τ, σ)) (∀ b: array(τ, σ))
    --   ((∀ i: τ) (select(a, i) = select(b, i)) => a = b)
    extensionality = forall (NE.two a' b')
                            (forall (NE.one i')
                                    (select a i === select b i) ==> a === b)

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

    select arr index = application (selectSymbol t) (NE.two arr index)
    store  arr index val = application (storeSymbol t) (NE.three arr index val)
