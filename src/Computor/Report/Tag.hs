{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Computor.Report.Tag
  ( Span(..)
  , Spanned(..)
  , spanned
  , fullyInLine
  , spannedBy
  , spanned1
  , spanned2
  ) where

import Prelude hiding (span)

import Computor.Parser

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Applicative

import Prettyprinter hiding (line)

-- TAGS

data Span =
  Span SourcePos SourcePos
  deriving (Show, Ord, Eq)

data Spanned a
  = At
  { span   :: Span
  , sValue :: a
  }
  deriving (Show, Eq, Functor)

instance Ord a => Ord (Spanned a) where
  compare (At _ a) (At _ b) =
    compare a b

at :: SourcePos -> SourcePos -> a -> Spanned a
at f t =
  At (Span f t)

inLine :: Pos -> SourcePos -> Bool
inLine l (SourcePos _ l' _) = l == l'

fullyInLine :: Pos -> Span -> Bool
fullyInLine l (Span f t) = inLine l f && inLine l t

instance Semigroup Span where
  (<>) (Span start _) (Span _ end) = Span start end

instance Monoid Span where
  -- Law holds because
  --   forall a. mempty <> a == a && a <> mempty == a

  -- NOTE: Assumes we don't care about filename here. Oh well.
  mempty = Span
           (SourcePos "" (mkPos maxBound) (mkPos maxBound))
           (SourcePos "" (mkPos 0) (mkPos 0))

spannedBy :: Spanned a -> Spanned b -> c -> Spanned c
spannedBy a b p =
  At (span a <> span b) p

spanned2 :: (Spanned a -> Spanned a ->         a)
         ->  Spanned a -> Spanned a -> Spanned a
spanned2 f x y = spannedBy x y (f x y)

spanned1 :: (Spanned a ->         a)
         ->  Spanned a -> Spanned a
spanned1 f x = At (span x) (f x)

-- USE IN PARSERS
spanned :: (MonadParsec e s m) => m a -> m (Spanned a)
spanned p =
  flip . at <$> getSourcePos <*> p <*> getSourcePos


-- PRETTY PRINTING

instance Pretty Pos where
  pretty = pretty . unPos

instance Pretty SourcePos where
  pretty (SourcePos filename l c) =
    pretty filename <> "[" <+> pretty l <> ":" <> pretty c <> "]"

instance Pretty Span where
  pretty (Span f t) =
    pretty f <+> "to" <+> pretty t

-- Tags should be transparent
instance Pretty a => Pretty (Spanned a) where
  pretty (At _ a) =
    pretty a
