module Kyckling.Program.Parse (parseAST) where

import System.IO
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Kyckling.Theory
import Kyckling.Parse
import qualified Kyckling.FOOL.Parse as F
import qualified Kyckling.Program.AST as AST

expr :: Parser AST.Expr
expr = do e <- expr'
          tern <- optionMaybe (reserved "?")
          case tern of
            Just _ -> do a <- expr
                         reserved ":"
                         b <- expr
                         return $ AST.Ternary e a b
            Nothing -> return e

expr' = buildExpressionParser operators term

operators = [ [prefix "-"  (AST.Unary  Negative)          ,
               prefix "+"  (AST.Unary  Positive)          ]
            , [binary "*"  (AST.Binary Multiply) AssocLeft]
            , [binary "+"  (AST.Binary Add     ) AssocLeft,
               binary "-"  (AST.Binary Subtract) AssocLeft]
            , [binary ">"  (AST.Binary Greater ) AssocNone,
               binary "<"  (AST.Binary Less    ) AssocNone,
               binary ">=" (AST.Binary Geq     ) AssocNone,
               binary "<=" (AST.Binary Leq     ) AssocNone]
            , [binary "=="  AST.Eql              AssocLeft,
               binary "!="  AST.InEql            AssocLeft]
            , [prefix "!"  (AST.Unary  Negate  )          ]
            , [binary "&&" (AST.Binary And     ) AssocLeft,
               binary "||" (AST.Binary Or      ) AssocLeft]
            ]

term =  parens expr
    <|> constant "true"  (AST.BoolConst True)
    <|> constant "false" (AST.BoolConst False)
    <|> AST.IntConst <$> integer
    <|> AST.LVal <$> lval

lval =  try (AST.ArrayElem <$> identifier <*> brackets expr)
    <|> AST.Var <$> identifier

stmts :: Parser [AST.Stmt]
stmts =  braces (many stmt)
     <|> (:[]) <$> stmt

stmt :: Parser AST.Stmt
stmt =  updateStmt
    <|> declareStmt
    <|> ifStmt

atomicStmt p = do { s <- p; semi; return s }

updateStmt = atomicStmt $ do lv <- lval
                             let unary  op = return (op lv)
                             let binary op = AST.Update lv op <$> expr
                             choice (map (update unary) unaryOps ++ map (update binary) binaryOps)
  where
    update u (t, op) = reserved t >> u op
    unaryOps  = [("++", AST.Increment), ("--", AST.Decrement)]
    binaryOps = [("=",  AST.Assign), ("+=", AST.Add), ("-=", AST.Subtract), ("*=", AST.Multiply)]

declareStmt = atomicStmt $ AST.Declare <$> typ <*> commaSep1 definition
  where
    definition = (,) <$> identifier <*> maybeBody
    maybeBody  = optionMaybe (reservedOp "=" >> expr)

ifStmt = reserved "if" >> AST.If <$> parens expr <*> stmts <*> elseStmts
  where
    elseStmts = fromMaybe [] <$> optionMaybe (reserved "else" >> stmts)

assert = atomicStmt (reserved "assert" >> AST.Assert <$> F.formula)

ast :: Parser AST.AST
ast = whiteSpace >> AST.AST <$> many stmt <*> many assert

parseAST :: SourceName -> String -> Either ParseError AST.AST
parseAST = parse ast