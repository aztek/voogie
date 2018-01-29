module Voogie.Parse where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Voogie.Theory

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

identifier = Token.identifier lexer
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

operator fix name fun = fix (reservedOp name >> return fun)
infix'  = operator Infix
prefix  = operator Prefix
postfix = operator Postfix

assocLeft = map ($ AssocLeft)
assocNone = map ($ AssocNone)

typ :: Parser Type
typ = atomicType <|> arrayType

atomicType =  constant "int"  Integer
          <|> constant "bool" Boolean

arrayType = Array <$> brackets (commaSep1 typ) <*> typ

typed :: Parser a -> Parser (Typed a)
typed p = do
  v <- p
  reservedOp ":"
  t <- typ
  return (Typed t v)
