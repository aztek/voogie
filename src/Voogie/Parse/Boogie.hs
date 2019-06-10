{-|
Module       : Voogie.Parse.Boogie
Description  : Parser of the Boogie language.
Copyright    : (c) Evgenii Kotelnikov, 2019
License      : GPL-3
Maintainer   : evgeny.kotelnikov@gmail.com
Stability    : provisional
-}

module Voogie.Parse.Boogie (
  module Voogie.Parse,
  expr,
  lval,
  stmt,
  declaration,
  returns,
  property,
  topLevel,
  main,
  boogie,
  parseBoogie
) where

import Control.Applicative ((<|>), many)
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE (zip)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Text.Parsec (SourceName, try, optionMaybe)
import Text.Parsec.Expr (Assoc, buildExpressionParser)

import Voogie.AST.Boogie
import Voogie.AST.FOOL (Formula)
import Voogie.Error (Result)
import Voogie.Parse
import qualified Voogie.Parse.FOOL as F
import Voogie.Language

expr :: Parser Expression
expr = buildExpressionParser operators term

unary :: UnaryOp -> Operator Expression
unary = prefix <$> nameOf <*> Unary

binary :: BinaryOp -> Assoc -> Operator Expression
binary = infix' <$> nameOf <*> Binary

equals :: Sign -> Assoc -> Operator Expression
equals = infix' <$> nameOf <*> Equals

operators :: [[Operator Expression]]
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

term :: Parser Expression
term =  parens expr
    <|> ast (BooleanLiteral <$> boolean)
    <|> ast (IntegerLiteral <$> integer)
    <|> ast (Ref <$> lval)
    <|> ifElse

ifElse :: Parser Expression
ifElse =  ast $ IfElse
      <$> (reserved kwdIf   *> expr)
      <*> (reserved kwdThen *> expr)
      <*> (reserved kwdElse *> expr)

lval :: Parser LValue
lval = LValue <$> identifier <*> many (brackets $ commaSep1 expr)

stmts :: Parser [Statement]
stmts =  braces (many stmt)
     <|> (:[]) <$> stmt

stmt :: Parser Statement
stmt = assignStmt <|> ifStmt

assignStmt :: Parser Statement
assignStmt = ast . atomicStmt $ do
  lvals <- commaSep1 lval
  reserved opAssign
  rvals <- commaSep1 expr
  guard (length lvals == length rvals)
  return $ Assign (NE.zip lvals rvals)

ifStmt :: Parser Statement
ifStmt = ast $ reserved kwdIf >> If <$> parens expr <*> stmts <*> elseStmts
  where elseStmts = fromMaybe [] <$> optionMaybe (reserved kwdElse *> stmts)

atomicStmt :: Parser a -> Parser a
atomicStmt p = p <* semi

keyword :: Name -> Parser a -> Parser a
keyword k p = atomicStmt (reserved k *> p)

declaration :: Parser Declaration
declaration = keyword kwdVar $ Declaration <$> typed (commaSep1 identifier)

main :: Parser Main
main = do
  sequence_ [reserved kwdProcedure, reserved kwdMain, parens $ return ()]
  rs <- optionMaybe returns
  ms <- optionMaybe modifies
  pre  <- many (try precondition)
  post <- many (try postcondition)
  (ds, ss) <- braces $ do
    ds <- many (try declaration)
    ss <- many topLevel
    return (ds, ss)
  return (Main (maybe [] toList ms) pre rs ds ss post)

returns :: Parser Returns
returns = reserved kwdReturns >> parens (Returns <$> commaSep1 (typed identifier))

modifies :: Parser (NonEmpty Identifier)
modifies = keyword kwdModifies (commaSep1 identifier)

precondition :: Parser Formula
precondition = keyword kwdRequires F.formula

postcondition :: Parser Formula
postcondition = keyword kwdEnsures F.formula

property :: Parser Property
property =  keyword kwdAssume (Assume <$> F.formula)
        <|> keyword kwdAssert (Assert <$> F.formula)

topLevel :: Parser (Either Statement Property)
topLevel = Left <$> stmt <|> Right <$> property

boogie :: Parser Boogie
boogie = whiteSpace >> Boogie <$> many (try declaration) <*> main

parseBoogie :: Maybe SourceName -> Text -> Result Boogie
parseBoogie = parse boogie
