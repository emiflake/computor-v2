{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Computor.AST.Identifier
  ( Identifier(..)
  , IdScope(..)
  )
where

import Data.Text (Text)

import Prettyprinter

data IdScope = STerm
  deriving (Show, Eq)

data Identifier (scope :: IdScope)
  = Identifier { unIdentifier :: Text }
  deriving (Show, Eq)

instance Pretty (Identifier scope) where
  pretty (Identifier ident) =
    pretty ident
