
module Language.Hafly.Stdlib where
import Language.Hafly.Interpreter
import Data.Dynamic
import Data.HashMap hiding(lookup, fromList)
import Data.MultiMap (fromList, MultiMap, fromMap, toMap, lookup)
import Control.Monad.Combinators.Expr
import Control.Applicative
import Data.Proxy
import Control.Monad (forM_)
import Type.Reflection (SomeTypeRep(SomeTypeRep))
import Data.Function ((&))
import Control.Category ((>>>))
import Data.IORef
import Control.Monad.Fix (fix)
import Prelude hiding (lookup)
import Data.List (intercalate)

base = InterpreterContext {
    exprDefs = fromList
        [
        -- Variables
          ("var", \_ -> toDyn $ newIORef @Dynamic)
        , ("get", \_ -> toDyn $ readIORef @Dynamic)
        -- Imperative helpers
        , ("loop", \_ -> toDyn $ \(x :: IO Dynamic) -> fix $ \(r :: IO Dynamic) -> x >> r)
        -- Type introspection
        , ("type", \_ -> toDyn dynTypeRep)
        -- Basic Console IO
        , ("printLn", \_ -> toDyn putStrLn)
        , ("readLn" , \_ -> toDyn getLine)
        -- Typeclass instances
        , ("show"   , \_ -> toDyn (show @SomeTypeRep))
        , ("show"   , \_ -> toDyn (show @Int))
        , ("show"   , \_ -> toDyn (show @String))
        , ("show"   , \_ -> toDyn (show @Double))
        , ("show"   , \_ -> toDyn (show @Bool))
        , ("show"   , \defs -> toDyn $ showListDyn defs)
        , ("show"   , \_ -> toDyn (show @(Map String Dynamic)))
        -- Higher order functions
        , ("forEach", \_ -> toDyn forEachList)
        , ("map"    , \_ -> toDyn (Prelude.map @Dynamic @Dynamic))
        , ("filter" , \_ -> toDyn dynFilter)
        , ("foldr"  , \_ -> toDyn (Prelude.foldr @[] @Dynamic @Dynamic))
        ]
  , operatorDefs =
        [
          [
            -- "dot" operator for universal function call syntax.
            InfixL $ Const ("." , toDyn ((\x y -> flexibleDynApply y x)))
          ],
          [
            InfixL $ Const ("of", toDyn ((.) @Dynamic @Dynamic @Dynamic))
          , InfixL $ Const ("then", toDyn ((>>>) @(->) @Dynamic @Dynamic @Dynamic))
          , InfixL $ Const (":=", toDyn (writeIORef @Dynamic))
          ],
          [
            InfixR $ Const ("*"  , toDyn ((*) @Int))
          , InfixR $ Const ("*"  , toDyn ((*) @Double))
          , InfixR $ Const ("/"  , toDyn (div @Int))
          , InfixR $ Const ("/"  , toDyn ((/) @Double))
          , InfixR $ Const ("mod", toDyn (mod @Int))
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
            InfixN $ Const ("==", toDyn ((==) @Int)),
            InfixN $ Const ("==", toDyn ((==) @Double)),
            InfixN $ Const ("==", toDyn ((==) @String)),
            InfixN $ Const ("==", toDyn ((==) @Bool))
          ]
        ]
  , monadDefs =
        [
          fromMonad $ Proxy @IO
        ]
}

showListDyn :: MultiMap String Dynamic -> [Dynamic] -> String
showListDyn defs xs = showStringList $
  (\x -> fromDyn (flattenDyn $ fromRight $ flexibleDynApp (singlyDispatched (lookup "show" defs)) x) "<DYN>") <$> xs

showStringList xs = "[" ++ intercalate ", " xs ++ "]"

forEachList :: [Dynamic] -> (Dynamic -> Dynamic) -> IO ()
forEachList xs f = forM_ xs $ \x -> do
  maybe (error "Function did not have expected IO result") id $
    toDynM (f x)

dynFilter :: (Dynamic -> Dynamic) -> [Dynamic] -> [Dynamic]
dynFilter f xs = Prelude.filter (\x -> fromDyn (f x) (error "Tried calling filter with function not returning boolean")) xs