
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
import Language.Hafly.Parser ( parseExpression )
import Control.Monad.Combinators.Expr
import Control.Applicative
import Data.Foldable
import Data.Type.Equality
import Data.Proxy

exampleContext = InterpreterContext {
    exprDefs = fromList
        [
          ("printLn", toDyn putStrLn)
        , ("readLn", toDyn getLine)
        , ("show", toDyn (show @Int))
        , ("show", toDyn (show @String))
        , ("show", toDyn (show @Double))
        ]
  , operatorDefs =
        [
          [
            InfixR $ Const ("*", toDyn ((*) @Int))
          , InfixR $ Const ("*", toDyn ((*) @Double))
          , InfixR $ Const ("/", toDyn (div @Int))
          , InfixR $ Const ("/", toDyn ((/) @Double))
          ],
          [
            InfixR $ Const ("+", toDyn ((+) @Int))
          , InfixR $ Const ("+", toDyn ((+) @Double))
          , InfixR $ Const ("-", toDyn ((-) @Int))
          , InfixR $ Const ("-", toDyn ((-) @Double))
          ],
          [
            InfixR $ Const ("$", toDyn flexibleDynApply)
          ]
        ]
  , monadDefs = [fromMonad $ Proxy @IO]
}

main :: IO ()
main = runInputT defaultSettings repl
  where
    repl :: InputT IO ()
    repl = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just input -> do
                case parseExpression (operatorDefs exampleContext) (T.pack input) of
                    Left err -> do
                        liftIO $ putStrLn "Error parsing input."
                        repl
                    Right exp -> do
                        case interpretIO exampleContext exp of
                            Just action -> liftIO action
                            Nothing -> do
                              case interpret exampleContext exp of
                                Left s -> error ""
                                Right result -> case tryShow exampleContext result of
                                    Nothing -> liftIO $ print result
                                    Just action -> liftIO action
                        repl

tryShow :: InterpreterContext -> Dynamic -> Maybe (IO ())
tryShow ctx@InterpreterContext {..} x = do
    showF  <- listToMaybe $ lookup "show" exprDefs
    result <- toMaybe $ flexibleDynApp showF x
    str <- asString result
    pure $ putStrLn str

asString :: Dynamic -> Maybe String
asString (Dynamic tr x) = case testEquality tr (typeRep @String) of
    Nothing   -> Nothing
    Just Refl -> Just x

toMaybe (Right x) = Just x
toMaybe (Left _) = Nothing
