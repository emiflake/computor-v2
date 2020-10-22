{-# LANGUAGE LambdaCase #-}
module Computor.REPL
  ( runRepl
  )
where

import qualified Computor.Pretty as Pretty

import System.Console.Haskeline

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Void
import Control.Monad

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Prettyprinter
import Prettyprinter.Util
import Prettyprinter.Render.Terminal

import Computor.Parser
import Computor.Trans
import Computor.Env
import Computor.Type.Checker
import Computor.AST.Parse
import Computor.AST
import Computor.Report.SourceCode
import qualified Computor.Report.Tag as Tag

import Control.Monad.Except

import qualified Data.Text as Text
import Data.Text (Text)

type REPL = ComputorT (InputT IO)

showEnvironment :: REPL ()
showEnvironment =
  liftIO $ putStrLn "Environment not implemented yet, even"

handleLine :: Text -> REPL ()
handleLine line = do
  expr <- parseStatement line
  liftIO . putDoc $
    "Parses as:" <+> prettyStatement expr <> hardline

  case expr of
    Tag.At _ (ExprQuery expr) -> do
        ((subst, ty), s) <- runCheckerT (infer expr)
        liftIO . putDoc $
          "Has type:" <+> pretty ty <> hardline
    _ -> liftIO $ putStrLn "This type of statement is TODO"
  
  pure ()

parseStatement :: Text -> REPL Statement
parseStatement line = do
  case runMyParser statement line of
    Left e -> throwError (SyntaxError e)
    Right v -> pure (desugarStatement v)

runRepl :: IO ()
runRepl =
  let
    loop :: REPL ()
    loop = do
      minput <- lift $ getInputLine (Pretty.renderToString Pretty.terminalLine)
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "env" -> showEnvironment >> loop
        Just xs | all (==' ') xs -> loop
        Just input -> do
          handleLine (Text.pack input)
            `catchError`
              \case
                 SyntaxError sError -> liftIO . putStrLn $ errorBundlePretty sError
                 TypeError e -> liftIO $ print e
                 RuntimeError -> liftIO $ putStrLn "Some runtime error happened"
          loop
  in
    void
  . runInputT defaultSettings
  . runComputorT (Environment Map.empty)
  $ loop
