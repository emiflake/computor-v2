{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Computor.AST.Identifier
  ( Identifier(..)
  , IdScope(..)
  )
where

import Data.Text (Text)

data IdScope = STerm
  deriving (Show, Eq)

data Identifier (scope :: IdScope)
  = Identifier Text
