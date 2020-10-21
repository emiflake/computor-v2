module Computor.REPL
  ( runRepl
  )
where

import qualified Computor.Pretty as Pretty

import System.Console.Haskeline

import Control.Monad.IO.Class

import Prettyprinter
import Prettyprinter.Util
import Prettyprinter.Render.Terminal

  
import Computor.Parser
import Computor.AST.Parse
import Computor.AST
import Computor.Report.SourceCode
import qualified Computor.Report.Tag as Tag

import qualified Data.Text as Text

runRepl :: IO ()
runRepl =
  let
    loop st = do
      minput <- getInputLine (Pretty.renderToString $ Pretty.terminalLine st)
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just xs | all (==' ') xs -> loop (st + 1)
        Just input -> do
          let src = Text.pack input
          case runMyParser statement src of
            Left e ->
              outputStrLn (errorBundlePretty e)
            Right v -> do
              liftIO $ putDoc (prettyStatement (desugarStatement v) <> hardline)
          loop (st + 1)
  in
  runInputT defaultSettings (loop (1 :: Int))
