module Voogie.Boogie.Parse (parseAST) where

import System.IO
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Voogie.Theory
import Voogie.Boogie.AST

import Voogie.Parse
import qualified Voogie.FOOL.Parse as F

expr :: Parser Expr
expr = buildExpressionParser operators term

operators = [ [prefix "-"  (Unary  Negative)          ,
               prefix "+"  (Unary  Positive)          ]
            , [binary "*"  (Binary Multiply) AssocLeft,
               binary "/"  (Binary Divide)   AssocLeft]
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
    <|> ifStmt

atomicStmt p = do { s <- p; semi; return s }

updateStmt = atomicStmt $ do lv <- lval
                             let unary  op = return (op lv)
                             let binary op = Update lv op <$> expr
                             choice (map (update unary) unaryOps ++ map (update binary) binaryOps)
  where
    update u (t, op) = reserved t >> u op
    unaryOps  = [("++", Increment), ("--", Decrement)]
    binaryOps = [(":=",  Assign), ("+=", Plus), ("-=", Minus), ("*=", Times)]

ifStmt = reserved "if" >> If <$> parens expr <*> stmts <*> elseStmts
  where
    elseStmts = fromMaybe [] <$> optionMaybe (reserved "else" >> stmts)

declaration = atomicStmt $ do reserved "var"
                              is <- commaSep1 identifier
                              reserved ":"
                              t <- typ
                              return $ Declare (Typed is t)

main = do reservedOp "procedure"
          reserved "main"
          reserved "("
          reserved ")"
          returns <- optionMaybe (atomicStmt $ reserved "returns" >> parens (Returns <$> typed))
          pre  <- many (try $ atomicStmt $ reservedOp "requires" >> parens (F.formula))
          post <- many (try $ atomicStmt $ reservedOp "ensures" >> parens (F.formula))
          (ds, ss) <- braces $ do ds <- many (try declaration)
                                  ss <- many stmt
                                  return (ds, ss)
          return (Main pre returns ds ss post)

--fundef = FunDef <$> typ <*> identifier <*> args <*> stmts
--  where
--    args = parens (commaSep arg)
--    arg  = flip Typed <$> typ <*> identifier

--assert = atomicStmt (reserved "assert" >> Assert <$> F.formula)

ast :: Parser AST
ast = whiteSpace >> AST <$> many (try declaration) <*> main

parseAST :: SourceName -> String -> Either ParseError AST
parseAST = parse ast
