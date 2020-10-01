module Computor.Util where

import Data.Text (Text)
import Computor.Parser
import Computor.AST.Parse

testParse :: Text -> IO ()
testParse src =
	case runMyParser expr src of
		Left e ->
			putStrLn (errorBundlePretty e)
		Right v ->
			print v
