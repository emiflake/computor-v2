module Computor.Term
  where


import Computor.Type.Rational
import Computor.Type.Matrix
import Computor.AST

import qualified Data.Text as Text
import Data.Text (Text)

data Term
  = TermReal Double
  | TermLam Text Expr
  | TermMatrix (Matrix Double)
  deriving (Show, Eq)
