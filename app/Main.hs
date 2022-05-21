
module Main where

import Data.Dynamic
import Data.Typeable
import Data.HashMap
import Prelude hiding (lookup)
import Control.Concurrent (yield)
import Data.Maybe
import Type.Reflection (SomeTypeRep(..))

data InterpreterData = InterpreterData {
    exprDefs  :: Map String Dynamic,
    operatorDefs :: Map String (Int, Dynamic)
}

data Ast =
    Atom String
  | Literal LiteralExpr
  | App Ast Ast
  | Sequence SequenceAst

data LiteralExpr =
    IntLit Int
  | DoubleLit Double
  | StringLit String
  | Record (Map String LiteralExpr)

data SequenceAst = SequenceAst [SequenceExpr]

data SequenceExpr =
      Expr Ast
    | BindExpr String Ast

type TypeError = String

interpret :: InterpreterData -> Ast -> Either TypeError Dynamic
interpret ctx@InterpreterData {..} = \case
    Atom x  -> maybe (Left "Could not find atom") Right $
        lookup x exprDefs
    App f' x' -> do
        f <- interpret ctx f'
        x <- interpret ctx x'

        let argType = case x of
                Dynamic z _ -> SomeTypeRep z
        let functionType = case f of
                Dynamic z _ -> SomeTypeRep z

        case funResultTy functionType argType of
            Nothing -> Left $ "Cannot apply function of type " ++
                show functionType ++ " to argument of type " ++
                    show argType
            Just resultType -> pure $ fromJust $
                dynApply f x
    Literal lit -> pure $ interpretLit lit
    _       -> undefined

interpretLit :: LiteralExpr -> Dynamic
interpretLit = \case
      (IntLit n) -> toDyn n
      (DoubleLit x) -> toDyn x
      (StringLit s) -> toDyn s
      (Record map) -> toDyn map

interpretSequence :: InterpreterData -> SequenceAst -> Either TypeError Dynamic
interpretSequence = undefined

exampleContext = InterpreterData {
    exprDefs = fromList
        [
          ("printLn", toDyn putStrLn)
        , ("readLn", toDyn getLine)
        ]
  , operatorDefs = fromList 
        [
          ("+", (1, toDyn ((+) :: Int -> Int -> Int)))
        , ("*", (1, toDyn ((+) :: Int -> Int -> Int)))
        , ("-", (1, toDyn ((+) :: Int -> Int -> Int)))
        , ("/", (1, toDyn ((+) :: Int -> Int -> Int)))
        ]
}

exampleAst = App (Atom "printLn")
    (Literal $ StringLit "Hello, world!")

interpretIO :: InterpreterData -> Ast -> IO ()
interpretIO ctx ast = do
    let result = interpret ctx ast
    case result of
        Left err -> putStrLn err
        Right x ->
            case fromDynamic @(IO ()) x of
                Nothing -> putStrLn "Error: Expression was not of type IO ()"
                Just action -> action

main :: IO ()
main = interpretIO
    exampleContext
    exampleAst

