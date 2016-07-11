module Kyckling.Program.Parse (parseAST) where

import System.IO
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Kyckling.Theory
import Kyckling.Program.AST

import Kyckling.Parse
import qualified Kyckling.FOOL.Parse as F

expr :: Parser Expr
expr = do e <- expr'
          tern <- optionMaybe (reserved "?")
          case tern of
            Just _ -> do a <- expr
                         reserved ":"
                         b <- expr
                         return (Ternary e a b)
            Nothing -> return e

expr' = buildExpressionParser operators term

operators = [ [prefix "-"  (Unary  Negative)          ,
               prefix "+"  (Unary  Positive)          ]
            , [binary "*"  (Binary Multiply) AssocLeft]
            , [binary "+"  (Binary Add     ) AssocLeft,
               binary "-"  (Binary Subtract) AssocLeft]
            , [binary ">"  (Binary Greater ) AssocNone,
               binary "<"  (Binary Less    ) AssocNone,
               binary ">=" (Binary Geq     ) AssocNone,
               binary "<=" (Binary Leq     ) AssocNone]
            , [binary "=="  Eql              AssocLeft,
               binary "!="  InEql            AssocLeft]
            , [prefix "!"  (Unary  Negate  )          ]
            , [binary "&&" (Binary And     ) AssocLeft,
               binary "||" (Binary Or      ) AssocLeft]
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
stmt =  updateStmt
    <|> declareStmt
    <|> ifStmt
    <|> returnStmt

atomicStmt p = do { s <- p; semi; return s }

updateStmt = atomicStmt $ do lv <- lval
                             let unary  op = return (op lv)
                             let binary op = Update lv op <$> expr
                             choice (map (update unary) unaryOps ++ map (update binary) binaryOps)
  where
    update u (t, op) = reserved t >> u op
    unaryOps  = [("++", Increment), ("--", Decrement)]
    binaryOps = [("=",  Assign), ("+=", Plus), ("-=", Minus), ("*=", Times)]

declareStmt = atomicStmt $ Declare <$> typ <*> commaSep1 definition
  where
    definition = (,) <$> identifier <*> maybeBody
    maybeBody  = optionMaybe (reservedOp "=" >> expr)

ifStmt = reserved "if" >> If <$> parens expr <*> stmts <*> elseStmts
  where
    elseStmts = fromMaybe [] <$> optionMaybe (reserved "else" >> stmts)

returnStmt = atomicStmt $ reserved "return" >> Return <$> expr

fundef = FunDef <$> typ <*> identifier <*> args <*> stmts
  where
    args = parens (commaSep1 arg)
    arg  = flip Typed <$> typ <*> identifier

assert = atomicStmt (reserved "assert" >> Assert <$> F.formula)

ast :: Parser AST
ast = whiteSpace >> AST <$> many fundef <*> many stmt <*> many assert

parseAST :: SourceName -> String -> Either ParseError AST
parseAST = parse ast