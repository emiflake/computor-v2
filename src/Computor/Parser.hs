module Computor.Parser
  ( Parser
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  )
where

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text (Text)
import Data.Void (Void)

type Parser = Parsec Void Text
--                   ^    ^
--                   |    \ Token stream
--                   |
--                   \ Custom error type
