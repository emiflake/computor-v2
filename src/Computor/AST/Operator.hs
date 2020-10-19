{-# LANGUAGE LambdaCase #-}
module Computor.AST.Operator
  ( Operator(..)
  )
where

import Prettyprinter

data Operator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | Power
  | Compose
  deriving (Show, Eq)

instance Pretty Operator where
  pretty = \case
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
    Modulus -> "%"
    Power -> "^"
    Compose -> "."
