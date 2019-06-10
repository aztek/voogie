module Voogie.Parse.FOOL (
  term,
  formula
) where

import Control.Applicative ((<|>), many)

import Text.Parsec.Expr (Assoc, buildExpressionParser)

import Voogie.AST.FOOL
import Voogie.Parse
import Voogie.Language

term :: Parser Term
term = buildExpressionParser operators arg

unary :: UnaryOp -> Operator Term
unary = prefix <$> nameOf <*> Unary

binary :: BinaryOp -> Assoc -> Operator Term
binary = infix' <$> nameOf <*> Binary

equals :: Sign -> Assoc -> Operator Term
equals = infix' <$> nameOf <*> Equals

operators :: [[Operator Term]]
operators = unaryOperators ++ binaryOperators
  where
    unaryOperators = fmap (\op -> unary <$> [op]) [minBound..]
    binaryOperators = [
        assocLeft $ binary <$> [Multiply, Divide],
        assocLeft $ binary <$> [Add, Subtract],
        assocNone $ binary <$> [Greater, Less, Geq, Leq],
        assocNone $ equals <$> [Pos, Neg],
        assocLeft $ binary <$> [And, Or],
        assocNone $ binary <$> [Imply],
        assocLeft $ binary <$> [Iff]
      ]

arg :: Parser Term
arg =  parens term
   <|> quantify
   <|> ast (BooleanConstant <$> boolean)
   <|> ast (IntegerConstant <$> integer)
   <|> ref
   <|> ifElse

ref :: Parser Term
ref = ast $ Ref <$> identifier <*> many (brackets $ commaSep1 term)

ifElse :: Parser Term
ifElse =  ast $ IfElse
      <$> (reserved kwdIf   *> term)
      <*> (reserved kwdThen *> term)
      <*> (reserved kwdElse *> term)

quantify :: Parser Formula
quantify =  ast $ Quantify
        <$> quantifier
        <*> commaSep1 (typed $ commaSep1 identifier) <* reserved opQsep
        <*> term

formula :: Parser Formula
formula = term
