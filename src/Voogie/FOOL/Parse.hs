module Voogie.FOOL.Parse (term, formula) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

import Voogie.Theory
import Voogie.Parse
import Voogie.FOOL.AST

import Voogie.BoogieSyntax

term :: Parser Term
term = buildExpressionParser operators arg

unary  = prefix <$> unaryOpName <*> Unary
binary = infix' <$> binaryOpName <*> Binary
equals = infix' <$> signName <*> Equals

operators = unaryOperators ++ binaryOperators
  where
    unaryOperators = fmap (\op -> unary <$> [op]) [minBound..]
    binaryOperators = [
        assocLeft $ binary <$> [Multiply, Divide],
        assocLeft $ binary <$> [Add, Subtract],
        assocNone $ binary <$> [Greater, Less, Geq, Leq],
        assocNone $ equals <$> [Pos, Neg],
        assocLeft $ binary <$> [And, Or],
        assocLeft $ binary <$> [Iff],
        assocNone $ binary <$> [Imply]
      ]

arg =  parens term
   <|> ast quantified
   <|> ast (BoolConst <$> boolean)
   <|> ast (IntConst <$> integer)
   <|> ast (Ref <$> identifier <*> many (brackets $ commaSep1 term))
   <|> ast (Ternary <$> (reserved kwdIf   >> term)
                    <*> (reserved kwdThen >> term)
                    <*> (reserved kwdElse >> term))

quantified =  Quantified
          <$> quantifier
          <*> commaSep1 (typed $ commaSep1 identifier) <* reserved "::"
          <*> term

formula :: Parser Formula
formula = term
