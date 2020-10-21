{-# LANGUAGE ViewPatterns #-}
module Computor.Util where

import Data.Text (Text)
import Computor.Parser
import Computor.AST.Parse
import Computor.AST
import Computor.Report.SourceCode
import qualified Computor.Report.Tag as Tag

import Prettyprinter
import Prettyprinter.Util
import Prettyprinter.Render.Terminal

testParse :: Text -> IO ()
testParse src =
  case runMyParser statement src of
    Left e ->
      putStrLn (errorBundlePretty e)
    Right v -> do
      let (Tag.At span _) = v
      putDoc (prettySpanSquiggly 100 span src <> hardline)
      putDoc (prettyStatement (desugarStatement v) <> hardline)

