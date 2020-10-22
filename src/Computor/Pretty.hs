module Computor.Pretty
  ( keyword
  , number
  , identifier
  , terminalLine
  , renderToString
  )
where

import Prettyprinter
import Prettyprinter.Render.Terminal

import qualified Data.Text as Text

-- TODO: maybe use this sometime in the future, if I want a bonus?

keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword =
  annotate bold . annotate (color Magenta)

number :: Doc AnsiStyle -> Doc AnsiStyle
number =
  annotate (color Blue)
 
identifier :: Doc AnsiStyle -> Doc AnsiStyle
identifier =
  annotate (color Yellow)

terminalLine :: Doc AnsiStyle
terminalLine =
  keyword ("Computor V2") <+> "λ> "

renderToString :: Doc AnsiStyle -> String
renderToString = Text.unpack . renderStrict . layoutPretty defaultLayoutOptions
