{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Computor.Env
  ( Environment(..)
  , lookup
  , lookupType
  , lookupTerm
  , store
  )
  where

import Prelude hiding (lookup)

import Computor.Type
import Computor.Term

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Functor

import qualified Data.Text as Text
import Data.Text (Text)

data Environment
  = Environment
  { getEnvironment :: Map Text (Type, Term)
  }
  deriving (Show)

lookup :: Text -> Environment -> Maybe (Type, Term)
lookup key = Map.lookup key . getEnvironment

lookupType :: Text -> Environment -> Maybe Type
lookupType key = fmap fst . lookup key

lookupTerm :: Text -> Environment -> Maybe Term
lookupTerm key = fmap snd . lookup key

store :: Text -> (Type, Term) -> Environment -> Environment
store key value = Environment . Map.insert key value . getEnvironment
