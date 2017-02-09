module Voogie.Boogie.Parse (parseAST) where

import System.IO
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

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

lval =  try (ArrayElem <$> identifier <*> brackets expr)
    <|> Var <$> identifier

stmts :: Parser [Stmt]
stmts =  braces (many stmt)
     <|> (:[]) <$> stmt

stmt :: Parser Stmt
stmt =  assignStmt
    <|> ifStmt

atomicStmt p = do { s <- p; semi; return s }

assignStmt = atomicStmt $ do lvals <- commaSep1 lval
                             reserved ":="
                             rvals <- commaSep1 expr
                             return $ Assign lvals rvals

ifStmt = reserved "if" >> If <$> parens expr <*> stmts <*> elseStmts
  where
    elseStmts = fromMaybe [] <$> optionMaybe (reserved "else" >> stmts)

declaration = atomicStmt $ do reserved "var"
                              is <- commaSep1 identifier
                              reserved ":"
                              t <- typ
                              return $ Declare (Typed is t)

assume = atomicStmt $ do reserved "assume"
                         f <- F.formula
                         return $ Assume f

main = do reserved "procedure"
          reserved "main"
          reserved "("
          reserved ")"
          returns <- optionMaybe (reserved "returns" >> parens (Returns <$> typed identifier))
          _ <- optionMaybe (atomicStmt $ reserved "modifies" >> commaSep1 identifier)
          pre  <- many (try precondition)
          post <- many (try postcondition)
          (ds, ss) <- braces $ do ds <- many (try declaration)
                                  ss <- many (Left <$> stmt <|> Right <$> assume)
                                  return (ds, ss)
          return (Main pre returns ds ss post)
  where
    precondition  = atomicStmt (reservedOp "requires" >> F.formula)
    postcondition = atomicStmt (reservedOp "ensures"  >> F.formula)

ast :: Parser AST
ast = whiteSpace >> AST <$> many (try declaration) <*> main

parseAST :: SourceName -> String -> Either ParseError AST
parseAST = parse ast
