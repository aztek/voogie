module Voogie.Boogie.Parse (
  parseAST
) where

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

lval :: Parser LVal
lval = Ref <$> identifier <*> many (brackets $ commaSep1 expr)

stmts :: Parser [Stmt]
stmts =  braces (many stmt)
     <|> (:[]) <$> stmt

stmt :: Parser Stmt
stmt = assignStmt <|> ifStmt

assignStmt = atomicStmt $ do
  lvals <- commaSep1 lval
  reserved ":="
  rvals <- commaSep1 expr
  guard (length lvals == length rvals)
  return $ Assign (NE.zip lvals rvals)

ifStmt = reserved "if" >> If <$> parens expr <*> stmts <*> elseStmts
elseStmts = fromMaybe [] <$> optionMaybe (reserved "else" >> stmts)

atomicStmt p = do { s <- p; _ <- semi; return s }

keyword k p = atomicStmt (reserved k >> p)

decl :: Parser Decl
decl = keyword "var" $ Declare <$> typed (commaSep1 identifier)

main :: Parser Main
main = do
  mapM_ reserved ["procedure", "main", "(", ")"]
  returns <- optionMaybe returns
  _ <- optionMaybe modifies
  pre  <- many (try precondition)
  post <- many (try postcondition)
  (ds, ss) <- braces $ do
    ds <- many (try decl)
    ss <- many topLevel
    return (ds, ss)
  return (Main pre returns ds ss post)

returns = reserved "returns" >> parens (Returns <$> commaSep1 (typed identifier))
modifies = keyword "modifies" (commaSep1 identifier)

precondition  = keyword "requires" F.formula
postcondition = keyword "ensures"  F.formula

assume = keyword "assume" (Assume <$> F.formula)

topLevel = Left <$> stmt <|> Right <$> assume

ast :: Parser AST
ast = whiteSpace >> AST <$> many (try decl) <*> main

parseAST :: SourceName -> String -> Either ParseError AST
parseAST = parse ast
