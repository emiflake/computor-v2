{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Computor.AST.Parse
where

import Computor.Parser
import Computor.AST
import Computor.AST.Identifier
import qualified Computor.AST.Operator as Op
import Computor.Report

import Data.Foldable (asum)

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

-- PRIMITIVE PARSERS

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reservedKeywords :: [Text]
reservedKeywords = []

identifier ::
  forall (scope :: IdScope) .
     Parser Char
  -> Parser Char
  -> Parser (Identifier scope)
identifier first rest =
  fmap Identifier $
    Text.cons
    <$> first
    <*> (Text.pack <$> many rest)

termIdentifier :: Parser (Identifier 'STerm)
termIdentifier =
  identifier lowerChar alphaNumChar

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = char '\"' *> (Text.pack <$> manyTill L.charLiteral (char '\"'))

float :: Parser Double
float = lexeme L.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negate
    ]
  , [ binary "*" (BinOp Op.Multiply)
    , binary "/" (BinOp Op.Divide)
    , binary "%" (BinOp Op.Modulus)
    ]
  , [ binary "+" (BinOp Op.Add)
    , binary "-" (BinOp Op.Subtract)
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr') -> Operator Parser Expr
binary name f = InfixL (spanned2 f <$ symbol name)

prefix :: Text -> (Expr -> Expr') -> Operator Parser Expr
prefix name f = Prefix (spanned1 f <$ symbol name)

-- COMPOUNDS


lambda :: Parser Expr
lambda =
  spanned $
    Lam 
    <$  symbol "\\"
    <*> termIdentifier
    <*  symbol "->"
    <*> expr

application :: Parser Expr
application =
  spanned $
  asum
  [ App
    <$> (spanned $ LitIdent <$> termIdentifier)
    <*  symbol "("
    <*> expr
    <*  symbol ")"
  , App
    <$  symbol "("
    <*> expr
    <*  symbol ")"
    <*  symbol "("
    <*> expr
    <*  symbol ")"
  ]

term :: Parser Expr
term =
  asum
  [ parens expr
  , spanned $ LitIdent <$> termIdentifier
  , spanned $ LitNum <$> float
  , lambda
  , application
  ]

expr :: Parser Expr
expr =
  makeExprParser term operatorTable