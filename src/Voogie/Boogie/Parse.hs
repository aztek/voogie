module Voogie.Boogie.Parse where

import Control.Monad
import Data.Maybe
import qualified Data.List.NonEmpty as NE

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

import Voogie.Error
import Voogie.Theory
import Voogie.BoogieSyntax
import Voogie.Boogie.AST

import Voogie.Parse
import qualified Voogie.FOOL.Parse as F

expr :: Parser Expr
expr = buildExpressionParser operators term

unary  = prefix <$> unaryOpName <*> Unary
binary = infix' <$> binaryOpName <*> Binary
equals = infix' <$> signName <*> Equals

operators = unaryOperators ++ binaryOperators
  where
    unaryOperators = fmap (\op -> unary <$> [op]) [minBound..]
    binaryOperators = [
        assocLeft $ binary <$> [Multiply, Divide],
        assocLeft $ binary <$> [Add, Subtract],
        assocNone $ binary <$> [Greater, Less, Geq, Leq],
        assocNone $ equals <$> [Pos, Neg],
        assocLeft $ binary <$> [And, Or],
        assocLeft $ binary <$> [Iff, Xor],
        assocNone $ binary <$> [Imply]
      ]

term =  parens expr
    <|> ast (BoolConst <$> boolean)
    <|> ast (IntConst <$> integer)
    <|> ast (LVal <$> lval)

lval :: Parser LVal
lval = Ref <$> identifier <*> many (brackets $ commaSep1 expr)

stmts :: Parser [Stmt]
stmts =  braces (many stmt)
     <|> (:[]) <$> stmt

stmt :: Parser Stmt
stmt = ast (assignStmt <|> ifStmt)

assignStmt = atomicStmt $ do
  lvals <- commaSep1 lval
  reserved opAssign
  rvals <- commaSep1 expr
  guard (length lvals == length rvals)
  return $ Assign (NE.zip lvals rvals)

ifStmt = reserved kwdIf >> If <$> parens expr <*> stmts <*> elseStmts
elseStmts = fromMaybe [] <$> optionMaybe (reserved kwdElse >> stmts)

atomicStmt p = p <* semi

keyword k p = atomicStmt (reserved k >> p)

decl :: Parser Decl
decl = keyword kwdVar $ Declare <$> typed (commaSep1 identifier)

main :: Parser Main
main = do
  sequence_ [reserved kwdProcedure, reserved kwdMain, parens $ return ()]
  returns <- optionMaybe returns
  ms <- optionMaybe modifies
  pre  <- many (try precondition)
  post <- many (try postcondition)
  (ds, ss) <- braces $ do
    ds <- many (try decl)
    ss <- many topLevel
    return (ds, ss)
  return (Main (maybe [] NE.toList ms) pre returns ds ss post)

returns = reserved kwdReturns >> parens (Returns <$> commaSep1 (typed identifier))
modifies = keyword kwdModifies (commaSep1 identifier)

precondition  = keyword kwdRequires F.formula
postcondition = keyword kwdEnsures  F.formula

assume = keyword kwdAssume (Assume <$> F.formula)

topLevel = Left <$> stmt <|> Right <$> assume

boogie = whiteSpace >> Boogie <$> many (try decl) <*> main

rewrapParsingError :: Parser a -> SourceName -> String -> Result a
rewrapParsingError p sn s = case parse p sn s of
  Left e -> Left (ParsingError e)
  Right b -> Right b

parseBoogie :: SourceName -> String -> Result Boogie
parseBoogie = rewrapParsingError boogie
