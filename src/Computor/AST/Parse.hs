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
import qualified Computor.Type.Matrix as Matrix
import Computor.Type.Matrix (Matrix)

import Data.Foldable (asum)
import Data.Functor
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))

import Control.Applicative (Alternative)
import qualified Control.Applicative as Applicative

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
reservedKeywords = [ "i" ]

identifier ::
     Parser Char
  -> Parser Char
  -> Parser Identifier
identifier first rest =
  fmap Identifier $ lexeme
    (Text.cons
    <$> first
    <*> (Text.pack <$> many rest))

keyword :: Text -> Parser Text
keyword match = lexeme (string match <* notFollowedBy alphaNumChar)

termIdentifier :: Parser Identifier
termIdentifier =
  identifier lowerChar alphaNumChar

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = char '\"' *> (Text.pack <$> manyTill L.charLiteral (char '\"'))

float :: Parser Double
float = try (lexeme L.float) <|> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

matrix :: Parser a -> Parser (Matrix a)
matrix p =
  fmap Matrix.unsafeFromLists $ squareBrackets
    (sepBy1
      (squareBrackets
        (sepBy1 p (symbol ",")))
      (symbol ";"))


operatorTable :: [[Operator Parser SExpr]]
operatorTable =
  [ [ prefix "-" SNegate
    ]
  , [ binary "^" (SBinOp Op.Power)
    ]
  , [ binary "*" (SBinOp Op.Multiply)
    , binary "/" (SBinOp Op.Divide)
    , binary "%" (SBinOp Op.Modulus)
    ]
  , [ binary "+" (SBinOp Op.Add)
    , binary "-" (SBinOp Op.Subtract)
    ]
  , [ binary "." (SBinOp Op.Compose) ]
  ]

binary :: Text -> (SExpr -> SExpr -> SExpr') -> Operator Parser SExpr
binary name f = InfixL (spanned2 f <$ symbol name)

prefix :: Text -> (SExpr -> SExpr') -> Operator Parser SExpr
prefix name f = Prefix (spanned1 f <$ symbol name)

sepByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty p sep = (:|) <$> p <*> Applicative.many (sep *> p)


-- COMPOUNDS


lambda :: Parser SExpr
lambda =
  spanned $
    SLam
    <$  symbol "\\"
    <*> sepByNonEmpty termIdentifier (symbol ",")
    <*  symbol "->"
    <*> expr

application :: Parser SExpr
application =
  spanned $
  asum
  [ SApp -- f(x)
    <$> (spanned $ SLitIdent <$> termIdentifier)
    <*> parens (sepByNonEmpty expr (symbol ","))
  , SApp -- (... y ...)(x)
    <$> parens expr
    <*> parens (sepByNonEmpty expr (symbol ","))
  ]

term :: Parser SExpr
term =
  asum
  [ try application
  , parens expr
  , spanned $ SLitMatrix <$> matrix expr
  , try $ spanned $ SLitImag <$ keyword "i"
  , spanned $ SLitIdent <$> termIdentifier
  , spanned $ SLitNum <$> float
  , lambda
  ]

expr :: Parser SExpr
expr =
  makeExprParser term operatorTable


-- STATEMENTS


statement :: Parser SStatement
statement =
  -- Why should `try` be used here?
  -- Unfortunately, all of these statements are kind of mutually consuming a first ident
  -- See:
  -- - Assignment consumes with <lhs:ident> = ...
  -- - Function definion consumes with <lhs:ident>(...) = ...
  -- - Expr query consumes with <lhs:expr> = ?
  -- (expr itself can consume identifier by itself, thus it can fail too)
    sc *> (spanned $ asum
    [ try assignment
    , try functionDefinition
    , exprQuery
    ]) <* sc

-- TODO: FIXME: TODO: FOR REAL: fix indentation parsing
-- e.g.
-- SHOULD BE VALID:
-- ```
-- foo(bar) =
--   42 * bar
-- ```
-- SHOULD NOT BE VALID:
-- ```
-- foo(bar) =
-- 42 * bar
-- ```
-- Look at
-- https://bit.ly/37nmd5i "indentation-sensitive-parsing.md"
functionDefinition :: Parser SStatement'
functionDefinition =
  SFunctionDefinition
  <$> termIdentifier
  <*  symbol "("
  <*> sepByNonEmpty termIdentifier (symbol ",")
  <*  symbol ")"
  <*  symbol "="
  <*> expr

assignment :: Parser SStatement'
assignment =
  SAssignment
  <$> termIdentifier
  <*  symbol "="
  <*> expr

exprQuery :: Parser SStatement'
exprQuery =
  SExprQuery <$> expr <*
  asum
  [ symbol "=" <* symbol "?"
  , symbol "?"
  ]
