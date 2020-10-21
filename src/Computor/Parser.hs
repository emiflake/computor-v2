{-# LANGUAGE RecordWildCards #-}
module Computor.Parser
  ( Parser
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , runMyParser
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

runMyParser :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runMyParser parser =
  parse parser "<stdio>"

-- TODO: maybe remove this? Probably not useful.
-- Instead, we should just shift error messages using Tag.shiftLine
setLine :: Pos -> Parser ()
setLine newLinePos =
  updateParserState
    (\state@State{..} ->
       state
       { statePosState =
         statePosState
         { pstateSourcePos =
             (pstateSourcePos statePosState)
             { sourceLine = newLinePos
             }
         }
       })
