module Computor.AST.Operator
  ( Operator(..)
  )
where

data Operator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  deriving (Show, Eq)
