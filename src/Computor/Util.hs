module Computor.Util where

import Data.Text (Text)
import Computor.Parser
import Computor.AST.Parse
import Computor.AST

import Prettyprinter
import Prettyprinter.Util

testParse :: Text -> IO ()
testParse src =
  case runMyParser statement src of
    Left e ->
      putStrLn (errorBundlePretty e)
    Right v -> do
      putStrLn "-- Parsed"
      putDocW 40 (pretty v <> hardline)
      putStrLn "-- Desugared"
      putDocW 40 (pretty (desugarStatement v) <> hardline)
