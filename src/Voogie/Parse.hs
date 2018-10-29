module Voogie.Parse where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import Voogie.AST
import Voogie.Theory
import Voogie.BoogieSyntax

language = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.reservedNames   = [ "var"
                            , "if", "else"
                            , "true", "false"
                            , "int", "bool"
                            , "procedure", "modifies", "returns"
                            , "assert", "assume", "requires", "ensures"
                            ]
  , Token.reservedOpNames = [ "+",  "-",  "*", "div"
                            , "+=", "-=", "*=", ":=", "++", "--"
                            , "==", "!=", "<", ">", "<=", ">="
                            , "&&", "||", "!", "==>"
                            , ":", "?", "::"
                            , "[]"
                            ]
  }

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

constant name fun = reserved name >> return fun

infix' name fun = Infix $ do
  reservedOp name
  return $ \a b -> AST (fst $ position a, snd $ position b) (fun a b)

prefix name fun = Prefix $ do
  AST (b, _) _ <- ast (reservedOp name)
  return $ \a -> AST (b, snd $ position a) (fun a)

postfix name fun = Postfix $ do
  AST (_, e) _ <- ast (reservedOp name)
  return $ \a -> AST (fst $ position a, e) (fun a)

assocLeft = map ($ AssocLeft)
assocNone = map ($ AssocNone)

ast :: Parser a -> Parser (AST a)
ast p = do
  begin <- getPosition
  a <- p
  end <- getPosition
  return $ AST (begin, end) a

typ :: Parser Type
typ = atomicType <|> arrayType

atomicType =  constant "int"  Integer
          <|> constant "bool" Boolean

arrayType = Array <$> brackets (commaSep1 typ) <*> typ

typed :: Parser a -> Parser (Typed a)
typed a = flip Typed <$> a <* reservedOp ":" <*> typ

boolean :: Parser Bool
boolean = constant (booleanName True)  True
      <|> constant (booleanName False) False

quantifier :: Parser Quantifier
quantifier =  constant (quantifierName Forall) Forall
          <|> constant (quantifierName Exists) Exists
