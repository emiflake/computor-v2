{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Computor.Trans
  ( ComputorT(..)
  , runComputorT
  , runComputor
  , ComputorError(..)
  , envLookup
  , envLookupType
  , envLookupTerm
  , envStore
  )
  where

import Computor.Env as Env
import Computor.Parser
import Computor.Error
import Computor.Term
import Computor.Type

import Data.Functor.Identity
import Data.Void
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Trans.Class

import qualified Data.Text as Text
import Data.Text (Text)

newtype ComputorT m a =
  ComputorT
  { runComputorT' ::
      ExceptT ComputorError
        (StateT Environment m) a
  }
  deriving
    ( Monad
    , Functor
    , Applicative
    , MonadError ComputorError
    , MonadState Environment
    )

deriving instance MonadIO m => MonadIO (ComputorT m)

instance MonadTrans ComputorT where
  lift = ComputorT . lift . lift

type Computor = ComputorT Identity

runComputorT :: Environment -> ComputorT m a -> m (Either ComputorError a, Environment)
runComputorT env =
  (`runStateT` env) . runExceptT . runComputorT'

runComputor :: Environment -> Computor a -> (Either ComputorError a, Environment)
runComputor env = runIdentity . runComputorT env


envLookup :: Monad m => Text -> ComputorT m (Maybe (Type, Term))
envLookup key = Env.lookup key <$> get

envLookupType :: Monad m => Text -> ComputorT m (Maybe Type)
envLookupType key = Env.lookupType key <$> get

envLookupTerm :: Monad m => Text -> ComputorT m (Maybe Term)
envLookupTerm key = Env.lookupTerm key <$> get

envStore :: Monad m => Text -> (Type, Term) -> ComputorT m ()
envStore key value = modify (Env.store key value)
