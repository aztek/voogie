module Voogie.Parse where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Voogie.Theory

language = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.reservedNames   = [ "if", "else"
                            , "true", "false"
                            , "int", "bool"
                            , "procedure", "var"
                            , "assert", "returns", "requires", "ensures"
                            , "return"
                            ]
  , Token.reservedOpNames = [ "+",  "-",  "*", "/"
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
commaSep1  = Token.commaSep1  lexer

constant name fun = reserved name >> return fun

operator fix name fun = fix (reservedOp name >> return fun)
binary  = operator Infix
prefix  = operator Prefix
postfix = operator Postfix

typ :: Parser Type
typ =  atomicTyp
   <|> Array <$> brackets typ <*> typ

atomicTyp =  constant "int"  Integer
         <|> constant "bool" Boolean

typed :: Parser (Typed String)
typed = do i <- identifier
           reservedOp ":"
           t <- typ
           return (Typed i t)