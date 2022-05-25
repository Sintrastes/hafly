
module Main where

import Data.Dynamic
import Data.Typeable
import Data.HashMap
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

exampleContext = InterpreterData {
    exprDefs = fromList
        [
          ("printLn", toDyn putStrLn)
        , ("readLn", toDyn getLine)
        , ("toString", toDyn (show :: Int -> String))
        ]
  , operatorDefs = 
        [
          [
            InfixR $ Const ("*", toDyn ((*) :: Int -> Int -> Int))
          , InfixR $ Const ("/", toDyn (div :: Int -> Int -> Int))
          ],
          [
            InfixR $ Const ("+", toDyn ((+) :: Int -> Int -> Int))
          , InfixR $ Const ("-", toDyn ((-) :: Int -> Int -> Int))
          ],
          [
            InfixR $ Const ("$", toDyn flexibleDynApply)
          ]
        ]
  , monadDefs = [fromMonad $ Proxy @IO]
}

exampleAst = App (Atom "printLn")
    (Literal $ StringLit "Hello, world!")

exampleArithmetic = App (Atom "printLn") $
    App (Atom "toString") $
        App (App (Atom "+")
            (Literal $ IntLit 2))
            (Literal $ IntLit 2)

exampleSimpleSeq :: Ast
exampleSimpleSeq = Sequence $  SequenceAst
    [
      Expr $ App (Atom "printLn") (Literal $ StringLit "Hello... (wait for it)")
    , Expr $ App (Atom "printLn") (Literal $ StringLit "World!")
    ]

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
                        liftIO $ interpretIO exampleContext exp
                        repl

