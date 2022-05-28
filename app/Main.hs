
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
import Control.Exception

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
                                Right result -> liftIO $
                                    tryShow exampleContext result
                                        (print result)
                        repl

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
