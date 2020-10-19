module Computor.Util where

import Data.Text (Text)
import Computor.Parser
import Computor.AST.Parse
import Computor.AST

import Prettyprinter

testParse :: Text -> IO ()
testParse src =
  case runMyParser statement src of
    Left e ->
      putStrLn (errorBundlePretty e)
    Right v -> do
      putStrLn "-- Parsed"
      print (pretty v)
      putStrLn "-- Desugared"
      print (pretty (desugarStatement v))
