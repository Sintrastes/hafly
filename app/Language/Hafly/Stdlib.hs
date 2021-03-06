
module Language.Hafly.Stdlib where
import Language.Hafly.Interpreter
import Data.Dynamic
import Data.HashMap hiding(fromList)
import Data.MultiMap (fromList)
import Control.Monad.Combinators.Expr
import Control.Applicative
import Data.Proxy
import Control.Monad (forM_)

base = InterpreterContext {
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
            InfixN $ Const ("==", toDyn ((==) @Int)),
            InfixN $ Const ("==", toDyn ((==) @Double)),
            InfixN $ Const ("==", toDyn ((==) @String)),
            InfixN $ Const ("==", toDyn ((==) @Bool))
          ]
        ]
  , monadDefs = [fromMonad $ Proxy @IO]
}

forEachList :: [Dynamic] -> (Dynamic -> IO Dynamic) -> IO ()
forEachList = forM_