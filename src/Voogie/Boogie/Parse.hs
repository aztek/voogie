module Voogie.Boogie.Parse (parseAST) where

import Control.Monad (guard)
import Data.Maybe
import qualified Data.List.NonEmpty as NE

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Voogie.Theory
import Voogie.Boogie.AST

import Voogie.Parse
import qualified Voogie.FOOL.Parse as F

expr :: Parser Expr
expr = buildExpressionParser operators term

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
               binary "||"  (Binary Or      ) AssocLeft]
            ]

term =  parens expr
    <|> constant "true"  (BoolConst True)
    <|> constant "false" (BoolConst False)
    <|> IntConst <$> integer
    <|> LVal <$> lval

lval = Ref <$> identifier <*> many (brackets $ commaSep1 expr)

stmts :: Parser [Stmt]
stmts =  braces (many stmt)
     <|> (:[]) <$> stmt

stmt :: Parser Stmt
stmt =  assignStmt
    <|> ifStmt

atomicStmt p = do { s <- p; _ <- semi; return s }

assignStmt = atomicStmt $ do lvals <- commaSep1 lval
                             reserved ":="
                             rvals <- commaSep1 expr
                             guard (length lvals == length rvals)
                             return $ Assign (NE.zip lvals rvals)

keyword k p = atomicStmt $ reserved k >> p

ifStmt = reserved "if" >> If <$> parens expr <*> stmts <*> elseStmts
  where
    elseStmts = fromMaybe [] <$> optionMaybe (reserved "else" >> stmts)

declaration = keyword "var" $ Declare <$> typed (commaSep1 identifier)

assume = keyword "assume" $ Assume <$> F.formula

main = do mapM_ reserved ["procedure", "main", "(", ")"]
          returns <- optionMaybe (reserved "returns" >> parens (Returns <$> commaSep1 (typed identifier)))
          _ <- optionMaybe (keyword "modifies" $ commaSep1 identifier)
          pre  <- many (try precondition)
          post <- many (try postcondition)
          (ds, ss) <- braces $ do ds <- many (try declaration)
                                  ss <- many (Left <$> stmt <|> Right <$> assume)
                                  return (ds, ss)
          return (Main pre returns ds ss post)
  where
    precondition  = keyword "requires" F.formula
    postcondition = keyword "ensures"  F.formula

ast :: Parser AST
ast = whiteSpace >> AST <$> many (try declaration) <*> main

parseAST :: SourceName -> String -> Either ParseError AST
parseAST = parse ast
