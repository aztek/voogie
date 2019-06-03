module Voogie.Parse (
  Parser,
  Operator,
  identifier,
  reserved,
  reservedOp,
  parens,
  integer,
  semi,
  whiteSpace,
  braces,
  brackets,
  commaSep1,
  infix',
  prefix,
  postfix,
  assocLeft,
  assocNone,
  ast,
  constant,
  typ,
  typed,
  constants,
  boolean,
  quantifier
) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.List.NonEmpty as NE (fromList)
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable (asum)

import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim (Parsec, getPosition)
import Text.Parsec.Expr (Assoc(..))
import qualified Text.Parsec.Expr as E (Operator(..))
import qualified Text.Parsec.Token as Token

import Voogie.AST
import Voogie.Theory
import Voogie.BoogieSyntax

type Parser = Parsec Text ()
type Operator = E.Operator Text () Identity

language :: Monad m => Token.GenLanguageDef Text u m
language = Token.LanguageDef {
  Token.commentStart    = "/*",
  Token.commentEnd      = "*/",
  Token.commentLine     = "//",
  Token.nestedComments  = True,
  Token.identStart      = letter,
  Token.identLetter     = alphaNum <|> oneOf "_'",
  Token.opStart         = Token.opLetter language,
  Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~",
  Token.reservedNames   = keywords
                       ++ quantifiers
                       ++ booleans,
  Token.reservedOpNames = operatorNames
                       ++ unaryOps
                       ++ binaryOps
                       ++ signs,
  Token.caseSensitive   = True
} where
  quantifiers = nameOf <$> [minBound :: Quantifier ..]
  booleans    = nameOf <$> [minBound :: Bool ..]
  unaryOps    = nameOf <$> [minBound :: UnaryOp ..]
  binaryOps   = nameOf <$> [minBound :: BinaryOp ..]
  signs       = nameOf <$> [minBound :: Sign ..]

lexer :: Token.GenTokenParser Text u Identity
lexer = Token.makeTokenParser language

identifier :: Parser (AST Name)
identifier = ast $ Token.identifier lexer

reserved :: Name -> Parser ()
reserved = Token.reserved lexer

reservedOp :: Name -> Parser (AST ())
reservedOp = ast . Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

integer :: Parser Integer
integer = Token.integer lexer

semi :: Parser Name
semi = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

commaSep1 :: Parser a -> Parser (NonEmpty a)
commaSep1 p = NE.fromList <$> Token.commaSep1 lexer p

(<+>) :: AST a -> AST b -> (SourcePos, SourcePos)
AST (a, _) _ <+> AST (_, b) _ = (a, b)

infix' :: Name -> (AST a -> AST a -> a) -> Assoc -> Operator (AST a)
infix' n f = E.Infix . fmap (\_ a b -> AST (a <+> b) (f a b)) $ reservedOp n

prefix :: Name -> (AST a -> a) -> Operator (AST a)
prefix n f = E.Prefix . fmap (\op x -> AST (op <+> x) (f x)) $ reservedOp n

postfix :: Name -> (AST a -> a) -> Operator (AST a)
postfix n f = E.Postfix . fmap (\op x -> AST (x <+> op) (f x)) $ reservedOp n

assocLeft :: [Assoc -> b] -> [b]
assocLeft = map ($ AssocLeft)

assocNone :: [Assoc -> b] -> [b]
assocNone = map ($ AssocNone)

ast :: Parser a -> Parser (AST a)
ast p = do
  begin <- getPosition
  a <- p
  end <- getPosition
  return $ AST (begin, end) a

constant :: Name -> b -> Parser b
constant name fun = reserved name $> fun

typ :: Parser Type
typ =  constant typeInteger Integer
   <|> constant typeBoolean Boolean
   <|> Array <$> brackets (commaSep1 typ) <*> typ

typed :: Parser a -> Parser (Typed a)
typed a = flip Typed <$> a <* reservedOp opTyped <*> typ

constants :: (Enum a, Bounded a, Named a) => Parser a
constants = asum $ fmap (constant =<< nameOf) [minBound..]

boolean :: Parser Bool
boolean = constants

quantifier :: Parser Quantifier
quantifier = constants
