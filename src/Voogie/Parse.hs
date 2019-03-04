module Voogie.Parse where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable

import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token

import Voogie.AST
import Voogie.Theory
import Voogie.BoogieSyntax

language = Token.LanguageDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = Token.opLetter language
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = keywords
                         ++ names quantifierName
                         ++ names booleanName
  , Token.reservedOpNames = operatorNames
                         ++ names unaryOpName
                         ++ names binaryOpName
                         ++ names signName
  , Token.caseSensitive   = True
  }
  where
    names p = p <$> [minBound..]

lexer = Token.makeTokenParser language

identifier = ast (Token.identifier lexer)
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer
braces     = Token.braces     lexer
brackets   = Token.brackets   lexer
commaSep   = Token.commaSep   lexer

commaSep1 :: Parser a -> Parser (NonEmpty a)
commaSep1 p = NE.fromList <$> Token.commaSep1 lexer p

AST (a, _) _ <+> AST (_, b) _ = (a, b)

infix' name fun = Infix $ do
  reservedOp name
  return $ \a b -> AST (a <+> b) (fun a b)

prefix name fun = Prefix $ do
  op <- ast (reservedOp name)
  return $ \x -> AST (op <+> x) (fun x)

postfix name fun = Postfix $ do
  op <- ast (reservedOp name)
  return $ \x -> AST (x <+> op) (fun x)

assocLeft = map ($ AssocLeft)
assocNone = map ($ AssocNone)

ast :: Parser a -> Parser (AST a)
ast p = do
  begin <- getPosition
  a <- p
  end <- getPosition
  return $ AST (begin, end) a

constant name fun = reserved name >> return fun

typ :: Parser Type
typ = constant typeInteger Integer
  <|> constant typeBoolean Boolean
  <|> Array <$> brackets (commaSep1 typ) <*> typ

typed :: Parser a -> Parser (Typed a)
typed a = flip Typed <$> a <* reservedOp opTyped <*> typ

constants :: (Enum a, Bounded a) => (a -> String) -> Parser a
constants p = asum $ fmap (constant =<< p) [minBound..]

boolean :: Parser Bool
boolean = constants booleanName

quantifier :: Parser Quantifier
quantifier = constants quantifierName
