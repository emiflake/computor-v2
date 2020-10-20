{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Computor.AST
  ( SStatement'(..)
  , SStatement
  , desugarStatement
  , Statement'(..)
  , Statement
  , Expr
  , Expr'(..)
  , desugarExpr
  , SExpr
  , SExpr'(..)
  )
where

import Computor.AST.Identifier (Identifier, IdScope(..))
import Computor.AST.Operator (Operator(..))
import Computor.Type.Matrix

import qualified Computor.Report.Tag as Tag

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Prettyprinter


type SStatement = Tag.Spanned SStatement'

data SStatement'
  -- Assignment to current scope
  -- e.g. <name:ident> = <expr>
  = SAssignment (Identifier 'STerm) SExpr

  -- Function definition sugar, desugars to lambda binding
  -- e.g. <name:ident>(<variables:ident>, ...) = <expr>
  -- desugars to <name:ident> = \<variables:ident> -> <expr>
  | SFunctionDefinition
      (Identifier 'STerm)
      (NonEmpty (Identifier 'STerm))
      SExpr

  -- Expression query
  -- <expr> = ?
  | SExprQuery SExpr

desugarStatement :: SStatement -> Statement
desugarStatement =
  \case
    Tag.At s (SAssignment ident expr) ->
      Tag.At s $ Assignment ident (desugarExpr expr)
    Tag.At s (SFunctionDefinition name variables expr) ->
      Tag.At s $ Assignment name
        (desugarExpr (Tag.At s $ SLam variables expr))
    Tag.At s (SExprQuery expr) ->
      Tag.At s $ ExprQuery (desugarExpr expr)


type Statement = Tag.Spanned Statement'

data Statement'
  = Assignment (Identifier 'STerm) Expr
  | ExprQuery Expr

-- The source of AST that is parsesd
type SExpr = Tag.Spanned SExpr'

data SExpr'
  -- A literal number, e.g. 42.1337
  = SLitNum Double

  -- A literal identifier for terms, e.g. fooBar
  | SLitIdent (Identifier 'STerm)

  -- A literal matrix, containing doubles, e.g. [[1,0];[0,1]]
  | SLitMatrix (Matrix SExpr)

  -- A binary operator, e.g. _lhs + _rhs
  | SBinOp Operator SExpr SExpr

  -- Negation operator
  | SNegate SExpr

  -- A lambda introduction, e.g. \_ -> _
  | SLam (NonEmpty (Identifier 'STerm)) SExpr

  -- Application, e.g. _(_)
  | SApp SExpr (NonEmpty SExpr)
  deriving (Show)

-- DESUGAR

-- Desugared version of SExpr
type Expr = Tag.Spanned Expr'

data Expr'
  = LitNum Double
  | LitIdent (Identifier 'STerm)
  | LitMatrix (Matrix SExpr)
  | BinOp Operator Expr Expr
  | Negate Expr
  | Lam (Identifier 'STerm) Expr
  | App Expr Expr
  deriving (Show)

desugarExpr :: SExpr -> Expr
desugarExpr =
  \case
    Tag.At s (SLitNum n) ->
      Tag.At s $ LitNum n
    Tag.At s (SLitIdent ident) ->
      Tag.At s $ LitIdent ident
    Tag.At s (SLitMatrix matrix) ->
      Tag.At s $ LitMatrix matrix
    Tag.At s (SBinOp operator lhs rhs) ->
      Tag.At s $ BinOp operator (desugarExpr lhs) (desugarExpr rhs)
    Tag.At s (SNegate expr) ->
      Tag.At s $ Negate (desugarExpr expr)
    Tag.At s (SLam arguments expr) ->
      foldr
        (\lhs rhs -> Tag.At s (Lam lhs rhs))
        (desugarExpr expr)
        (NonEmpty.toList arguments)
    Tag.At s (SApp fun args) ->
      foldl1 (\lhs rhs -> Tag.At s (App lhs rhs))
        (fmap desugarExpr $ fun NonEmpty.<| args)


-- PRETTY-PRINTING


instance Pretty SStatement' where
  pretty = \case
    SAssignment binding expr ->
      pretty binding <+> "=" <+> pretty expr
    SFunctionDefinition binding argument expr ->
      pretty binding <> "(" <> pretty argument <> ")"
        <+> "=" <+> pretty expr
    SExprQuery expr ->
      pretty expr <+> "=" <+> "?"

instance Pretty Statement' where
  pretty = \case
    Assignment binding expr ->
      pretty binding <+> "=" <+> pretty expr
    ExprQuery expr ->
      pretty expr <+> "=" <+> "?"

instance Pretty SExpr' where
  pretty = \case
    SLitNum n -> pretty n
    SLitIdent ident -> pretty ident
    SLitMatrix matrix -> pretty matrix
    SBinOp operator lhs rhs ->
      deepOperator lhs <+> pretty operator <+> deepOperator rhs
    SNegate rhs ->
      "-" <> pretty rhs
    SLam binding body ->
      "\\" <> pretty binding <+> "->" <+> pretty body
    SApp function value ->
      deepOperator function <> "(" <> pretty value <> ")"
    where
      -- Put parentheses if deeper expression is also a binary operator
      deepOperator expr =
        case expr of
          Tag.At _ (SBinOp _ _ _) -> "(" <> pretty expr <> ")"
          _ -> pretty expr

instance Pretty Expr' where
  pretty = \case
    LitNum n -> pretty n
    LitIdent ident -> pretty ident
    LitMatrix matrix -> pretty matrix
    BinOp operator lhs rhs ->
      align $ hsep [ deepOperator lhs <> softline', pretty operator, deepOperator rhs ]
    Negate rhs ->
      "-" <> pretty rhs
    Lam binding body ->
      "\\" <> pretty binding <+> "->" <> softline <> nest 1 (pretty body)
    App function value ->
      deepOperator function <> "(" <> pretty value <> ")"
    where
      -- Put parentheses if deeper expression is also a binary operator
      deepOperator expr =
        case expr of
          Tag.At _ (BinOp _ _ _) -> "(" <> pretty expr <> ")"
          _ -> pretty expr
