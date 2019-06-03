module Voogie.FOOL.Parse (
  term,
  formula
) where

import Control.Applicative ((<|>), many)

import Text.Parsec.Expr (Assoc, buildExpressionParser)

import Voogie.FOOL.AST
import Voogie.Parse
import Voogie.Theory

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
   <|> quantified
   <|> ast (BoolConst <$> boolean)
   <|> ast (IntConst <$> integer)
   <|> ref
   <|> ternary

ref :: Parser Term
ref = ast $ Ref <$> identifier <*> many (brackets $ commaSep1 term)

ternary :: Parser Term
ternary =  ast $ Ternary
       <$> (reserved kwdIf   *> term)
       <*> (reserved kwdThen *> term)
       <*> (reserved kwdElse *> term)

quantified :: Parser Formula
quantified =  ast $ Quantified
          <$> quantifier
          <*> commaSep1 (typed $ commaSep1 identifier) <* reserved opQsep
          <*> term

formula :: Parser Formula
formula = term
