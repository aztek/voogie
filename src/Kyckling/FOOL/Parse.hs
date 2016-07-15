module Kyckling.FOOL.Parse (term, formula) where

import System.IO
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Kyckling.Theory
import Kyckling.Parse
import Kyckling.FOOL.AST

term :: Parser Term
term = do t <- term'
          tern <- optionMaybe (reserved "?")
          case tern of
            Just _ -> do a <- term
                         reserved ":"
                         b <- term
                         return $ Ternary t a b
            Nothing -> return t

term' = buildExpressionParser operators arg

operators = [ [prefix "-"  (Unary  Negative)          ,
               prefix "+"  (Unary  Positive)          ]
            , [binary "*"  (Binary Multiply) AssocLeft]
            , [binary "+"  (Binary Add     ) AssocLeft,
               binary "-"  (Binary Subtract) AssocLeft]
            , [binary ">"  (Binary Greater ) AssocNone,
               binary "<"  (Binary Less    ) AssocNone,
               binary ">=" (Binary Geq     ) AssocNone,
               binary "<=" (Binary Leq     ) AssocNone]
            , [binary "==" (Equals Pos     ) AssocLeft,
               binary "!=" (Equals Neg     ) AssocLeft]
            , [prefix "!"  (Unary  Negate  )          ]
            , [binary "&&" (Binary And     ) AssocLeft,
               binary "||" (Binary Or      ) AssocLeft,
               binary "=>" (Binary Imply   ) AssocLeft]
            ]

arg =  parens term
   <|> constant "true"  (BoolConst True)
   <|> constant "false" (BoolConst False)
   <|> quantified 
   <|> IntConst <$> integer
   <|> try (ArrayElem <$> identifier <*> brackets term)
   <|> Constant <$> identifier

quantified = do q <- quantifier
                vars <- parens (commaSep1 typedVar)
                t <- arg
                return $ Quantified q vars t

quantifier =  constant "forall" Forall
          <|> constant "exists" Exists

typedVar :: Parser (Typed Name)
typedVar = Typed <$> identifier <*> typ

formula :: Parser Formula
formula = term