
module Main where

import Data.Dynamic
import Data.Typeable
import Data.HashMap
import Prelude hiding (lookup)
import Control.Concurrent (yield)
import Data.Maybe
import Type.Reflection (SomeTypeRep(..))
import Language.Hafly.Interpreter
import Language.Hafly.Ast

exampleContext = InterpreterData {
    exprDefs = fromList
        [
          ("printLn", toDyn putStrLn)
        , ("readLn", toDyn getLine)
        , ("toString", toDyn (show :: Int -> String))
        ]
  , operatorDefs = fromList
        [
          ("+", (1, toDyn ((+) :: Int -> Int -> Int)))
        , ("*", (1, toDyn ((*) :: Int -> Int -> Int)))
        , ("-", (1, toDyn ((-) :: Int -> Int -> Int)))
        , ("/", (1, toDyn (div :: Int -> Int -> Int)))
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
main = interpretIO
    exampleContext
    exampleSimpleSeq


