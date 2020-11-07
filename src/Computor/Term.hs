{-# LANGUAGE LambdaCase #-}
module Computor.Term
  where

import Computor.Type.Rational
import Computor.Type.Matrix
import Computor.AST

import qualified Data.Text as Text
import Data.Text (Text)
import Text.Printf

data Term
  = TermReal Double
  | TermLam Text Expr
  | TermMatrix (Matrix Double)
  | TermForeign (Term -> IO Term)

instance Show Term where
  show = const "Term"

instance Eq Term where
  (==) = const . const $ False
