{-# LANGUAGE LambdaCase #-}
module Computor.Term
  where

import Prelude hiding (Rational)

import Computor.Type.Rational
import Computor.Type.Matrix
import Computor.AST
import Computor.AST.Identifier
import Computor.AST.Operator
  
import qualified Computor.Pretty as Pretty
import qualified Computor.Report.Tag as Tag

import qualified Data.Text as Text
import Data.Text (Text)
import Text.Printf

import Prettyprinter
import Prettyprinter.Render.Terminal

data Number
  = RealNumber Double
  | ImaginaryNumber
  | RationalNumber Rational
  | MatrixNumber (Matrix Term)

data Term
  = TNumber Number
  | TLam Identifier Term
  | TIdent Identifier
  | TApp Term Term
  | TBinOp Operator Term Term
  | TNegate Term
  | TForeign (Term -> IO Term)

instance Show Term where
  show = \case
    TNumber n -> printf "(TNumber %s)" . show $ prettyNumber n
    TLam id t -> printf "(TLam %s %s)" (show id) (show t)
    TIdent id -> printf "(TIdent %s)" (show id)
    TApp f x -> printf "(TApp %s %s)" (show f) (show x)
    TNegate x -> printf "(TNegate %s)" (show x)
    TBinOp op lhs rhs -> printf "(TBinOp %s %s %s)" (show op) (show lhs) (show rhs)
    TForeign _ -> printf "(TForeign <...>)"

fromExpr :: Expr -> Term
fromExpr (Tag.At s expr) =
  case expr of
    LitNum d -> TNumber (RealNumber d)
    LitIdent id -> TIdent id
    LitMatrix m -> TNumber . MatrixNumber . fmap fromExpr $ m
    LitImag -> TNumber ImaginaryNumber
    BinOp op lhs rhs -> TBinOp op (fromExpr lhs) (fromExpr rhs)
    Negate e -> TNegate (fromExpr e) -- TODO: change this into a std call?
    Lam varBinding body -> TLam varBinding (fromExpr body)
    App f x -> fromExpr f `TApp` fromExpr x


prettyTerm :: Term -> Doc AnsiStyle
prettyTerm term = case term of
    TNumber n -> Pretty.number (prettyNumber n)
    TIdent ident -> Pretty.identifier (pretty ident)
    TBinOp operator lhs rhs ->
      align $ hsep [ deepOperator lhs <> softline', pretty operator, deepOperator rhs ]
    TNegate rhs ->
      "-" <> prettyTerm rhs
    TLam binding body ->
      "\\" <> Pretty.identifier (pretty binding) <+> "->" <> softline <> nest 1 (prettyTerm body)
    TApp function value ->
      deepOperator function <> "(" <> prettyTerm value <> ")"
    where
      -- Put parentheses if deeper expression is also a binary operator
      deepOperator term' =
        case term' of
          (TBinOp _ _ _) -> "(" <> prettyTerm term' <> ")"
          _ -> prettyTerm term'

prettyNumber :: Number -> Doc AnsiStyle
prettyNumber number = case number of
  RealNumber d -> pretty d
  ImaginaryNumber -> "i"
  RationalNumber rat -> pretty rat
  MatrixNumber mat -> prettyMatrix Nothing prettyTerm mat
