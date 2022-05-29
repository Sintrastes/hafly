
module Main where

import Data.Dynamic
import Type.Reflection
import Data.MultiMap
import Prelude hiding (lookup)
import Control.Concurrent (yield)
import Data.Maybe
import Type.Reflection (SomeTypeRep(..))
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

exampleContext = InterpreterContext {
    exprDefs = fromList
        [
        -- IO Functions
          ("printLn", toDyn putStrLn)
        , ("readLn" , toDyn getLine)
        -- Typeclass instances
        , ("show"   , toDyn (show @Int))
        , ("show"   , toDyn (show @String))
        , ("show"   , toDyn (show @Double))
        , ("show"   , toDyn (show @Bool))
        , ("show"   , toDyn (show @[Dynamic]))
        , ("show"   , toDyn (show @(Map String Dynamic)))
        -- Higher order functions
        , ("forEach", toDyn forEachList)
        , ("map"    , toDyn (Prelude.map @Dynamic @Dynamic))
        , ("filter" , toDyn (Prelude.filter @Dynamic))
        ]
  , operatorDefs =
        [
          [
            InfixR $ Const ("*"  , toDyn ((*) @Int))
          , InfixR $ Const ("*"  , toDyn ((*) @Double))
          , InfixR $ Const ("/"  , toDyn (div @Int))
          , InfixR $ Const ("/"  , toDyn ((/) @Double))
          , InfixR $ Const ("and", toDyn (&&))
          , InfixR $ Const ("or" , toDyn (||))
          ],
          [
            InfixR $ Const ("+", toDyn ((+) @Int))
          , InfixR $ Const ("+", toDyn ((+) @Double))
          , InfixR $ Const ("+", toDyn ((++) @Dynamic))
          , InfixR $ Const ("-", toDyn ((-) @Int))
          , InfixR $ Const ("-", toDyn ((-) @Double))
          ],
          [
            InfixR $ Const ("$", toDyn flexibleDynApply),
            InfixN $ Const ("==", toDyn ((==) @Int))
          ]
        ]
  , monadDefs = [fromMonad $ Proxy @IO]
}

forEachList :: [Dynamic] -> (Dynamic -> IO Dynamic) -> IO ()
forEachList = forM_

main :: IO ()
main = runInputT defaultSettings (repl exampleContext)
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

tryShow :: InterpreterContext -> Dynamic -> IO () -> IO ()
tryShow ctx@InterpreterContext {..} x alt = catch (do
    case toMaybe $ dispatched "show" $ lookup "show" exprDefs of
      Nothing -> alt
      Just showF -> do
        -- Need to guard against excepions for now as
        --  dynApply can throw.
        -- Would be better to model this with Either in the 
        --  future.
        result <- catch
            (evaluate $ maybe alt putStrLn . asString . flattenDyn <$> flexibleDynApp showF x)
            (\(e :: SomeException) -> pure $ Left "")
        case result of
          Left s -> alt
          Right act -> act)
            (\(e :: SomeException) -> alt)


asString :: Dynamic -> Maybe String
asString (Dynamic tr x) = case testEquality tr (typeRep @String) of
    Nothing   -> Nothing
    Just Refl -> Just x

toMaybe (Right x) = Just x
toMaybe (Left _) = Nothing
