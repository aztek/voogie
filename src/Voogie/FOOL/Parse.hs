module Voogie.FOOL.Parse (term, formula) where

import System.IO
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Voogie.Theory
import Voogie.Parse
import Voogie.FOOL.AST

term :: Parser Term
term = buildExpressionParser operators arg

operators = [ [prefix "-"   (Unary  Negative)          ,
               prefix "+"   (Unary  Positive)          ]
            , [binary "*"   (Binary Multiply) AssocLeft]
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
   <|> constant "true"  (BoolConst True)
   <|> constant "false" (BoolConst False)
   <|> quantified 
   <|> IntConst <$> integer
   <|> try (ArrayElem <$> identifier <*> brackets term)
   <|> Const <$> identifier

quantified = do q <- quantifier
                vars <- commaSep1 typed
                reserved "::"
                t <- term
                return $ Quantified q vars t

quantifier =  constant "forall" Forall
          <|> constant "exists" Exists

formula :: Parser Formula
formula = term
