module Computor.Pretty
  ( keyword
  , number
  , identifier
  , arrow
  , terminalLine
  , renderToString
  )
where

import Prettyprinter
import Prettyprinter.Render.Terminal

import qualified Data.Text as Text

keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword =
  annotate bold . annotate (color Magenta)

number :: Doc AnsiStyle -> Doc AnsiStyle
number =
  annotate (color Blue)
 
identifier :: Doc AnsiStyle -> Doc AnsiStyle
identifier =
  annotate (color Yellow)

arrow :: Doc AnsiStyle -> Doc AnsiStyle
arrow =
  annotate (color White)

terminalLine :: Doc AnsiStyle
terminalLine =
  keyword ("Computor V2") <+> "λ> "

renderToString :: Doc AnsiStyle -> String
renderToString = Text.unpack . renderStrict . layoutPretty defaultLayoutOptions
