
module Main where

import Data.Dynamic
import Type.Reflection
import Data.MultiMap
import Prelude hiding (lookup)
import Control.Concurrent (yield)
import Data.Maybe
import Language.Hafly.Interpreter
import Language.Hafly.Ast hiding (Const)
import System.Console.Haskeline
import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.Hafly.Parser ( parseExpression, parseExprDef )
import Control.Monad.Combinators.Expr
import Control.Applicative
import Data.Foldable
import Data.Type.Equality
import Data.Proxy
import Control.Exception
import Text.Megaparsec.Error
import Data.HashMap (Map)
import qualified Language.Hafly.Stdlib as Hafly

main :: IO ()
main = runInputT defaultSettings (repl Hafly.base)
  where
    repl :: InterpreterContext -> InputT IO ()
    repl ctx = do
        minput <- getInputLine "> "
        case minput of
            Nothing ->
                return ()
            Just input -> do
                updatedCtx <- processReplInput ctx input
                repl updatedCtx

    processReplInput :: InterpreterContext -> String -> InputT IO InterpreterContext
    processReplInput ctx input = do
        case parseExprDef (operatorDefs ctx) (T.pack input) of
            Right (x, xDef) -> do
                case flattenDyn <$> interpretRec ctx x xDef of
                    Left s -> do
                        liftIO $ putStrLn s
                        return ctx
                    Right dy -> return (addDef ctx x dy)
            Left err -> do
                case parseExpression (operatorDefs ctx) (T.pack input) of
                    Left err -> do
                        liftIO $ putStrLn $ errorBundlePretty err
                    Right exp -> do
                        case interpretIO ctx exp of
                            Just action -> liftIO action
                            Nothing -> do
                                case interpret ctx exp of
                                    Left s -> liftIO $ putStrLn s
                                    Right result -> do
                                        liftIO $ tryShow ctx result
                                            (print result)
                return ctx

