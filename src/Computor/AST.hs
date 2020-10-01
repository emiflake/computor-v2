{-# LANGUAGE DataKinds #-}
module Computor.AST
  ( Stmt(..)
  , Expr
  , Expr'(..)
  )
where

import Computor.AST.Identifier (Identifier, IdScope(..))
import Computor.AST.Operator (Operator(..))

import qualified Computor.Report.Tag as Tag

data Stmt
  -- Assignment to current scope
  -- e.g. _var = _expr
  = Assignment (Identifier 'STerm) Expr

  -- Function definition sugar, desugars to lambda binding
  -- e.g. _a(_b) = _c
  -- desugars to _a = \_b -> _c
  | FunctionDef (Tag.Spanned (Identifier 'STerm))
                (Tag.Spanned (Identifier 'STerm))
                Expr

-- The source AST that is parsed.
type Expr = Tag.Spanned Expr'

data Expr'
  -- A literal number, e.g. 42.1337
  = LitNum Double

  -- A literal identifier for terms, e.g. fooBar
  | LitIdent (Identifier 'STerm)

  -- A binary operator, e.g. _lhs + _rhs
  | BinOp Operator Expr Expr

  -- Negation operator
  | Negate Expr

  -- A lambda introduction, e.g. \_ -> _
  | Lam (Identifier 'STerm) Expr

  -- Application, e.g. _(_)
  | App (Identifier 'STerm) Expr
