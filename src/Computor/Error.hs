module Computor.Error
  ( ComputorError(..)
  , TypeCheckerError(..)
  )
  where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void

import Computor.Parser
import Computor.Type

data TypeCheckerError
  = UnificationError Type Type -- Two types are not unifiable; e.g. Number ~ a
  | OccursCheck Text Type -- Type variable occurs in non-simple type; e.g. a ~ (a -> a)
  | MissingVariable Text -- Type variable could not be found in environment
  deriving (Show, Eq)

data ComputorError
  = TypeError TypeCheckerError
  | RuntimeError
  | SyntaxError (ParseErrorBundle Text Void)
  deriving (Show, Eq)

