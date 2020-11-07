{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Computor.AST.Identifier
  ( Identifier(..)
  )
where

import Data.Text (Text)

import Prettyprinter
data Identifier
  = Identifier { unIdentifier :: Text }
  deriving (Show, Eq)

instance Pretty Identifier where
  pretty (Identifier ident) =
    pretty ident
