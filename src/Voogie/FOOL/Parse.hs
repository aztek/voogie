module Voogie.FOOL.Parse (term, formula) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Voogie.Theory
import Voogie.Parse
import Voogie.FOOL.AST

term :: Parser Term
term = buildExpressionParser operators arg

unary  = fmap (\(n, op) -> prefix n (Unary op))
binary = fmap (\(n, op) -> infix' n (Binary op))
equals = fmap (\(n, op) -> infix' n (Equals op))

operators = [
  unary [("-", Negative), ("+", Positive)],
  assocLeft $ binary [("*", Multiply), ("div", Divide)],
  assocLeft $ binary [("+", Add), ("-", Subtract)],
  assocNone $ binary [ (">", Greater), ("<", Less), (">=", Geq), ("<=", Leq)],
  assocLeft $ equals [("==", Pos), ("!=", Neg)],
  unary [("!", Negate)],
  assocLeft $ binary [("&&", And), ("||", Or), ("==>", Imply)]
  ]

arg =  parens term
   <|> quantified
   <|> constant "true"  (BoolConst True)
   <|> constant "false" (BoolConst False)
   <|> IntConst <$> integer
   <|> Ref <$> identifier <*> many (brackets $ commaSep1 term)

quantified = do
  q <- quantifier
  vars <- commaSep1 (typed $ commaSep1 identifier)
  reserved "::"
  Quantified q vars <$> term

quantifier =  constant "forall" Forall
          <|> constant "exists" Exists

formula :: Parser Formula
formula = term
