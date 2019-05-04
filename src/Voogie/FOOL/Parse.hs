module Voogie.FOOL.Parse (
  term,
  formula
) where

import Text.Parsec
import Text.Parsec.Expr

import Voogie.Theory
import Voogie.Parse
import Voogie.FOOL.AST

import Voogie.BoogieSyntax

term :: Parser Term
term = buildExpressionParser operators arg

unary  = prefix <$> nameOf <*> Unary
binary = infix' <$> nameOf <*> Binary
equals = infix' <$> nameOf <*> Equals

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

arg =  parens term
   <|> ast quantified
   <|> ast (BoolConst <$> boolean)
   <|> ast (IntConst <$> integer)
   <|> ast ref
   <|> ast ternary

ref = Ref <$> identifier <*> many (brackets $ commaSep1 term)

ternary =  Ternary
       <$> (reserved kwdIf   *> term)
       <*> (reserved kwdThen *> term)
       <*> (reserved kwdElse *> term)

quantified =  Quantified
          <$> quantifier
          <*> commaSep1 (typed $ commaSep1 identifier) <* reserved opQsep
          <*> term

formula :: Parser Formula
formula = term
