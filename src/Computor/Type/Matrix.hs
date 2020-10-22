{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Computor.Type.Matrix
  ( Matrix(..)
  , get
  , unsafeGet
  , set
  , set_
  , unsafeSet
  , identityMatrix
  , toList
  , toListsRM
  , toListsCM
  , indexMatrix
  , filled
  , fromList
  , unsafeFromLists
  , prettyMatrix
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Text (Text)
import qualified Data.Text as Text

import Prettyprinter

-- LANGUAGE
--
-- Given matrix M:
-- [ a b c ]
-- [ d e f ]
-- [ g h i ]
--
-- RM = Row major
--   read as [a b c] [d e f] [g h i]
--
-- CM = Column major
--   read as [a d g] [b e h] [c f i]
--
-- 'm * n matrix' means
--   m rows
--   n columns
--
-- We use i to mean row, and j to mean column.

data Matrix a
  = Matrix
  { rows :: Int
  , columns :: Int
  , elements :: Vector a
  }
  deriving (Show, Eq)

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix m n producer =
  Matrix
  { rows = m
  , columns = n
  , elements =
    Vector.fromList
    [ producer (i, j)
    | i <- [0..m - 1]
    , j <- [0..n - 1]
    ]
  }

identityMatrix' :: a -> a -> Int -> Matrix a
identityMatrix' zero one size =
  matrix size size $ \(i, j) -> if i == j then one else zero

identityMatrix :: Int -> Matrix Double
identityMatrix =
  identityMatrix' 0 1

filled :: a -> Int -> Int -> Matrix a
filled value m n =
  matrix m n $ const value

indexMatrix :: Int -> Matrix (Int, Int)
indexMatrix size =
  matrix size size $ id

unsafeGet :: Int -> Int -> Matrix a -> a
unsafeGet i j Matrix{..} =
  elements Vector.! (i * columns + j)

get :: Int -> Int -> Matrix a -> Maybe a
get i j Matrix{..} | i < 0 || i >= rows || j < 0 || j >= columns = Nothing
get i j Matrix{..} =
  elements Vector.!? (i * columns + j)

unsafeSet :: Int -> Int -> a -> Matrix a -> Matrix a
unsafeSet i j v m@Matrix{..} =
  m{ elements = elements Vector.// [(i * columns + j, v)] }

set :: Int -> Int -> a -> Matrix a -> Maybe (Matrix a)
set i j _ m@Matrix{..} | i < 0 || i >= rows || j < 0 || j >= columns = Nothing
set i j v m@Matrix{..} =
  Just $ m{ elements = elements Vector.// [(i * columns + j, v)] }

-- Lossy set at index, if out of bounds, ignore.
set_ :: Int -> Int -> a -> Matrix a -> Matrix a
set_ i j _ m@Matrix{..} | i < 0 || i >= rows || j < 0 || j >= columns = m
set_ i j v m@Matrix{..} =
  m{ elements = elements Vector.// [(i * columns + j, v)] }

fromList :: Int -> Int -> [a] -> Matrix a
fromList m n =
  Matrix m n . Vector.fromList

unsafeFromLists :: [[a]] -> Matrix a
unsafeFromLists xs =
  let
    rows = length xs
    columns = length (head xs)
  in
 fromList rows columns (concat xs)

toList :: Matrix a -> [a]
toList m@Matrix{..} =
  [ unsafeGet i j m
  | i <- [0..rows - 1]
  , j <- [0..columns - 1]
  ]

toListsRM :: Matrix a -> [[a]]
toListsRM m@Matrix{..} =
  [ [ unsafeGet i j m
    | j <- [0..rows - 1]
    ]
  | i <- [0..rows - 1]
  ]

toListsCM :: Matrix a -> [[a]]
toListsCM m@Matrix{..} =
  [ [ unsafeGet i j m
    | i <- [0..columns - 1]
    ]
  | j <- [0..rows - 1]
  ]

instance Functor Matrix where
  fmap f m@Matrix{..} = m{elements = fmap f elements}

-- PRETTY PRINTING ROUTINE

prettyMatrix :: Maybe ann -> (a -> Doc ann) -> Matrix a -> Doc ann
prettyMatrix annLabel prettyElem m@Matrix{..} =
    let
      maximumElementSize =
        Vector.maximum . fmap (length . show . prettyElem) $ elements

      label =
        show rows <> " x " <> show columns

      labelWidth =
        length label + 2

      width =
        (maximumElementSize + 1) * columns + 1

      missing =
        max 0 (width - labelWidth)

      prettyElement v =
        let
          elementLength =
              length (show $ prettyElem v)
        in
        prettyElem v <> (pretty (replicate (max 0 (maximumElementSize - elementLength)) ' '))
    in
    align . vsep $
    [ "┌ " <> ((maybe id annotate annLabel) (pretty label)) <> " " <> (pretty $ replicate missing '─') <> "┐" ] <>
    [ "│" <+> (hsep [ prettyElement (unsafeGet i j m) | j <- [0..columns-1] ]) <>
        pretty (replicate (max 0 $ 0 - (width - labelWidth))' ') <+>
        "│"
      | i <- [0..rows - 1]] <>
    [ "└" <> (pretty $ replicate (max labelWidth width) '─') <> "┘" <> softline' ]


instance Pretty a => Pretty (Matrix a) where
  pretty = prettyMatrix Nothing pretty
