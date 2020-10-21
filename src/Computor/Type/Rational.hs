module Computor.Type.Rational
  ( Rational(..)
  , simplify
  , toDouble
  )
  where

import Prelude hiding (Rational)

import Prettyprinter

data Rational
  = Rational
  { numerator :: Integer
  , denominator :: Integer
  }
  deriving (Show, Eq)


simplify :: Rational -> Rational
simplify (Rational n d) =
  let
    g = gcd n d
  in
  Rational (n `div` g) (d `div` g)

toDouble :: Rational -> Double
toDouble (Rational n d) = fromInteger n / fromInteger d

instance Num Rational where
    (Rational na da) + (Rational nb db) =
      let
        d = lcm da db
      in
      simplify $ Rational (na * d `div` da + nb * d `div` db) d

    (Rational na da) - (Rational nb db) =
      let
        d = lcm da db
      in
      simplify $ Rational (na * d `div` da - nb * d `div` db) d

    (Rational na da) * (Rational nb db) =
      simplify $ Rational (na * nb) (db * da)

    negate (Rational n d) =
      Rational (negate n) d

    abs (Rational n d) =
      Rational (abs n) (abs d)

    signum (Rational n d) =
      Rational (signum (n * d)) 1

    fromInteger x = Rational x 1


instance Pretty Rational where
  pretty (Rational n d) =
    let
      maxTermWidth =
        max (length . show $ n)
            (length . show $ d)

      alignToWidth term =
        let
          selfWidth =
            length . show $ term

          missingPadding =
            (maxTermWidth - selfWidth)
        in
        pretty (replicate missingPadding ' ')
    in
    case d of
      1 -> pretty n
      _ ->
        align . vsep $
        [ pretty n <> alignToWidth n
        , pretty $ replicate maxTermWidth '─'
        , alignToWidth d <> pretty d
        ]
