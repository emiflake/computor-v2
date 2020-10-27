{-# LANGUAGE LambdaCase #-}
module Computor.Error
  ( ComputorError(..)
  , TypeCheckerError(..)
  , prettyTypeCheckerError
  )
  where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void

import Computor.Parser
import qualified Computor.Pretty as Pretty
import Computor.Type
import Computor.Report.SourceCode
import qualified Computor.Report.Tag as Tag

import Prettyprinter
import Prettyprinter.Render.Terminal

data TypeCheckerError
  = UnificationError Type Type -- Two types are not unifiable; e.g. Number ~ a
  | OccursCheck Text Type -- Type variable occurs in non-simple type; e.g. a ~ (a -> a)
  | MissingVariable Tag.Span Text -- Type variable could not be found in environment
  deriving (Show, Eq)

prettyTypeCheckerError :: Text ->TypeCheckerError -> Doc AnsiStyle
prettyTypeCheckerError srcCode = \case
  UnificationError t1@(Tag.At s1 _) t2@(Tag.At s2 _) ->
    vsep
    [ "Unification error when trying to unify" <+> prettyType t1 <+> "and" <+> prettyType t2 <> "."
    , prettySpanSquiggly 0 s1 srcCode
    , ""
    ]
  OccursCheck t ty@(Tag.At s1 _) ->
    vsep
    [ "Variable" <+> "`" <> Pretty.identifier (pretty t) <> "`" <+> "occurs in" <+> prettyType ty
    , prettySpanSquiggly 0 s1 srcCode
    , ""
    ]
  MissingVariable s t ->
    vsep
    [ "Variable" <+> "`" <> pretty t <> "`" <+> "is not in scope"
    , prettySpanSquiggly 0 s srcCode
    , ""
    ]


data ComputorError
  = TypeError TypeCheckerError
  | RuntimeError
  | SyntaxError (ParseErrorBundle Text Void)
  deriving (Show, Eq)
