module Voogie.Boogie.Parse where

import Control.Applicative ((<|>), many)
import Control.Monad (guard)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE (toList, zip)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Text.Parsec (SourceName, try, parse, optionMaybe)
import Text.Parsec.Expr (Assoc, buildExpressionParser)

import Voogie.AST.Boogie
import Voogie.AST.FOOL (Formula)
import Voogie.Error
import qualified Voogie.FOOL.Parse as F
import Voogie.Parse
import Voogie.Theory

expr :: Parser Expr
expr = buildExpressionParser operators term

unary :: UnaryOp -> Operator Expr
unary  = prefix <$> nameOf <*> Unary

binary :: BinaryOp -> Assoc -> Operator Expr
binary = infix' <$> nameOf <*> Binary

equals :: Sign -> Assoc -> Operator Expr
equals = infix' <$> nameOf <*> Equals

operators :: [[Operator Expr]]
operators = unaryOperators ++ binaryOperators
  where
    unaryOperators = fmap (\op -> unary <$> [op]) [minBound..]
    binaryOperators = [
        assocLeft $ binary <$> [Multiply, Divide],
        assocLeft $ binary <$> [Add, Subtract],
        assocNone $ binary <$> [Greater, Less, Geq, Leq],
        assocNone $ equals <$> [Pos, Neg],
        assocLeft $ binary <$> [And, Or],
        assocNone $ binary <$> [Imply],
        assocLeft $ binary <$> [Iff]
      ]

term :: Parser Expr
term =  parens expr
    <|> ast (BoolConst <$> boolean)
    <|> ast (IntConst <$> integer)
    <|> ast (LVal <$> lval)
    <|> ternary

ternary :: Parser Expr
ternary =  ast $ Ternary
       <$> (reserved kwdIf   *> expr)
       <*> (reserved kwdThen *> expr)
       <*> (reserved kwdElse *> expr)

lval :: Parser LVal
lval = Ref <$> identifier <*> many (brackets $ commaSep1 expr)

stmts :: Parser [Stmt]
stmts =  braces (many stmt)
     <|> (:[]) <$> stmt

stmt :: Parser Stmt
stmt = assignStmt <|> ifStmt

assignStmt :: Parser Stmt
assignStmt = ast . atomicStmt $ do
  lvals <- commaSep1 lval
  reserved opAssign
  rvals <- commaSep1 expr
  guard (length lvals == length rvals)
  return $ Assign (NE.zip lvals rvals)

ifStmt :: Parser Stmt
ifStmt = ast $ reserved kwdIf >> If <$> parens expr <*> stmts <*> elseStmts
  where elseStmts = fromMaybe [] <$> optionMaybe (reserved kwdElse *> stmts)

atomicStmt :: Parser a -> Parser a
atomicStmt p = p <* semi

keyword :: Name -> Parser a -> Parser a
keyword k p = atomicStmt (reserved k *> p)

decl :: Parser Decl
decl = keyword kwdVar $ Declare <$> typed (commaSep1 identifier)

main :: Parser Main
main = do
  sequence_ [reserved kwdProcedure, reserved kwdMain, parens $ return ()]
  rs <- optionMaybe returns
  ms <- optionMaybe modifies
  pre  <- many (try precondition)
  post <- many (try postcondition)
  (ds, ss) <- braces $ do
    ds <- many (try decl)
    ss <- many topLevel
    return (ds, ss)
  return (Main (maybe [] NE.toList ms) pre rs ds ss post)

returns :: Parser Returns
returns = reserved kwdReturns >> parens (Returns <$> commaSep1 (typed identifier))

modifies :: Parser (NonEmpty Identifier)
modifies = keyword kwdModifies (commaSep1 identifier)

precondition :: Parser Formula
precondition = keyword kwdRequires F.formula

postcondition :: Parser Formula
postcondition = keyword kwdEnsures F.formula

property :: Parser Prop
property =  keyword kwdAssume (Assume <$> F.formula)
        <|> keyword kwdAssert (Assert <$> F.formula)

topLevel :: Parser (Either Stmt Prop)
topLevel = Left <$> stmt <|> Right <$> property

boogie :: Parser Boogie
boogie = whiteSpace >> Boogie <$> many (try decl) <*> main

rewrapParsingError :: Parser a -> SourceName -> Text -> Result a
rewrapParsingError p sn s = fmapError ParsingError (parse p sn s)

parseBoogie :: SourceName -> Text -> Result Boogie
parseBoogie = rewrapParsingError boogie
