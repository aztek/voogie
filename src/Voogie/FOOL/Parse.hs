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
   <|> ref

quantified = do q <- quantifier
                vars <- parens (commaSep1 typedVar)
                t <- arg
                return $ Quantified q vars t

quantifier =  constant "forall" Forall
          <|> constant "exists" Exists

typedVar :: Parser (Typed Name)
typedVar = flip Typed <$> typ <*> identifier

ref = do i <- identifier
         args <- optionMaybe (parens $ commaSep term)
         case args of
           Just args -> return (FunApp i args)
           Nothing   -> return (Const i)

formula :: Parser Formula
formula = term
