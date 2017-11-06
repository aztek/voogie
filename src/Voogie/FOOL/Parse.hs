module Voogie.FOOL.Parse (term, formula) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Voogie.Theory
import Voogie.Parse
import Voogie.FOOL.AST

term :: Parser Term
term = buildExpressionParser operators arg

operators = [ [prefix "-"   (Unary  Negative)          ,
               prefix "+"   (Unary  Positive)          ]
            , [binary "*"   (Binary Multiply) AssocLeft,
               binary "div" (Binary Divide)   AssocLeft]
            , [binary "+"   (Binary Add     ) AssocLeft,
               binary "-"   (Binary Subtract) AssocLeft]
            , [binary ">"   (Binary Greater ) AssocNone,
               binary "<"   (Binary Less    ) AssocNone,
               binary ">="  (Binary Geq     ) AssocNone,
               binary "<="  (Binary Leq     ) AssocNone]
            , [binary "=="  (Equals Pos     ) AssocLeft,
               binary "!="  (Equals Neg     ) AssocLeft]
            , [prefix "!"   (Unary  Negate  )          ]
            , [binary "&&"  (Binary And     ) AssocLeft,
               binary "||"  (Binary Or      ) AssocLeft,
               binary "==>" (Binary Imply   ) AssocLeft]
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
