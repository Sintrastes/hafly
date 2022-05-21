
module Main where

import Data.Dynamic
import Data.Typeable
import Data.HashMap
import Prelude hiding (lookup)
import Control.Concurrent (yield)
import Data.Maybe
import Type.Reflection (SomeTypeRep(..))

data InterpreterData = InterpreterData {
    exprDefs     :: Map String Dynamic,
    operatorDefs :: Map String (Int, Dynamic),
    monadDefs    :: [DynamicMonad] 
}

data DynamicMonad = DynamicMonad {
    -- | The bind operation of the monad applied to dynamic types.
    dynBind   :: (Dynamic -> Dynamic) -> Dynamic -> Either TypeError Dynamic,
    -- | The return operation of the monad applied to dynamic types.
    dynReturn :: Dynamic -> Dynamic,
    -- | Helper function to check to see if an expression is in
    --    this monad.
    isOfType  :: TypeRep -> Bool 
}

-- | Build up a DynamicMonad from a monad.
fromMonad :: forall m. (Monad m, Typeable m) => Proxy m -> DynamicMonad
fromMonad _ = DynamicMonad {
    dynReturn = \x -> case x of
        Dynamic _ (v :: a) -> toDyn ((return @m) x),
    dynBind = \f x -> undefined,
    isOfType = undefined
}

addDef :: InterpreterData -> String -> Dynamic -> InterpreterData
addDef ctx name def = ctx { 
    exprDefs = exprDefs ctx <> singleton name def 
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
        lookup x (exprDefs <> fmap snd operatorDefs)
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
    Literal lit  -> pure $ interpretLit lit
    Sequence seq -> interpretSequence ctx seq

interpretLit :: LiteralExpr -> Dynamic
interpretLit = \case
      (IntLit n) -> toDyn n
      (DoubleLit x) -> toDyn x
      (StringLit s) -> toDyn s
      (Record map) -> toDyn map

interpretSequence :: InterpreterData -> SequenceAst -> Either TypeError Dynamic
interpretSequence ctx@InterpreterData {..} = \case
    SequenceAst [] -> Left "Cannot interpret an empty sequence"
    SequenceAst (x:xs) -> do
        m <- tryInferMonad ctx x
        interpretMonadicSequence ctx m (x:xs)

tryInferMonad :: InterpreterData -> SequenceExpr -> Either TypeError DynamicMonad
tryInferMonad = undefined

interpretMonadicSequence :: InterpreterData -> DynamicMonad -> [SequenceExpr] -> Either TypeError Dynamic
interpretMonadicSequence ctx@InterpreterData {..} m@DynamicMonad {..} = \case
    []     -> undefined
    (x:xs) -> undefined

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
        , ("*", (1, toDyn ((+) :: Int -> Int -> Int)))
        , ("-", (1, toDyn ((+) :: Int -> Int -> Int)))
        , ("/", (1, toDyn ((+) :: Int -> Int -> Int)))
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

exampleSimpleSeq = Sequence $  SequenceAst
    [
      Expr $ App (Atom "printLn") (Literal $ StringLit "Hello... (wait for it)")
    , Expr $ App (Atom "printLn") (Literal $ StringLit "World!")
    ]

-- | Interpret a Hafly expression in the IO monad.
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
    exampleSimpleSeq


