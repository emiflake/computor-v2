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
  deriving (Show, Eq)

lookup :: Text -> Environment -> Maybe (Type, Term)
lookup key = Map.lookup key . getEnvironment

lookupType :: Text -> Environment -> Maybe Type
lookupType key = fmap fst . lookup key

lookupTerm :: Text -> Environment -> Maybe Term
lookupTerm key = fmap snd . lookup key


-- FANCY DUMB MONAD STUFF BELOW

{-

newtype EnvT e m a =
  EnvT
  { runEnvT :: StateT (Map Text e) m a
  }
  deriving
    ( Monad
    , Functor
    , Applicative
    , MonadState (Map Text e)
    )

newtype Env e a =
  Env
  { runEnv :: State (Map Text e) a
  }
  deriving
    ( Monad
    , Functor
    , Applicative
    , MonadState (Map Text e)
    )

class Monad m => MonadEnv e m | m -> e where
  load :: Text -> m (Maybe e)
  load key = loadAll <&> Map.lookup key
  store :: Text -> e -> m ()
  store key value = overAll (Map.insert key value)

  loadAll :: m (Map Text e)
  storeAll :: Map Text e -> m ()
  overAll :: (Map Text e -> Map Text e) -> m ()
  overAll f = Computor.Env.loadAll <&> f >>= storeAll

instance MonadEnv e (Env e) where
  loadAll = get
  storeAll = put
  overAll = modify

instance Monad m => MonadEnv e (EnvT e m) where
  loadAll = get
  storeAll = put
  overAll = modify

instance MonadEnv e m => MonadEnv e (StateT s m) where
  loadAll = lift loadAll
  storeAll = lift . storeAll
  overAll = lift . overAll

instance MonadEnv e m => MonadEnv e (ExceptT s m) where
  loadAll = lift loadAll
  storeAll = lift . storeAll
  overAll = lift . overAll

instance MonadEnv e m => MonadEnv e (ReaderT s m) where
  loadAll = lift loadAll
  storeAll = lift . storeAll
  overAll = lift . overAll

update :: MonadEnv e m => Text -> (e -> e) -> m ()
update key f =
  load key >>= \case
    Nothing -> pure ()
    Just v -> store key (f v)

-- Perform environment modifications in restorable way
-- Causes Space leak proportional to the size of the environment.
locally :: MonadEnv e m => m a -> m a
locally f = do
  before <- loadAll
  res <- f
  storeAll before
  pure res

-}
