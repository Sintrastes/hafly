{-# LANGUAGE GADTs #-}

module Language.Hafly.Interpreter where
import Data.Dynamic
import Data.Data (Proxy)
import Language.Hafly.Ast hiding (App)
import Type.Reflection hiding (App)
import Data.HashMap
import Prelude hiding (lookup)
import Data.Maybe
import Type.Reflection hiding (TypeRep, typeRep, typeRepTyCon)
import Language.Hafly.Ast hiding(App)
import qualified Language.Hafly.Ast as Ast
import Data.Type.Equality
import Data.Typeable (funResultTy, typeRepArgs, gcast)
import Type.Reflection.Unsafe (mkTrApp)
import Unsafe.Coerce (unsafeCoerce)
import Data.Kind (Type)
import Control.Monad
import Control.Arrow

data InterpreterData = InterpreterData {
    exprDefs     :: Map String Dynamic,
    operatorDefs :: Map String (Int, Dynamic),
    monadDefs    :: [DynamicMonad]
}

data DynamicM = forall a m. (Monad m) => DynamicM (TypeRep a) (TypeRep m) (m a)

data DynamicMFor m = forall a. (Monad m) => DynamicMFor (TypeRep a) (m a)

toDynamicM :: forall m. (Monad m, Typeable m) => DynamicMFor m -> DynamicM
toDynamicM (DynamicMFor tr ma) = DynamicM tr (typeRep @m) ma

checkM :: forall m. (Monad m, Typeable m) => Dynamic -> Maybe (DynamicMFor m)
checkM (Dynamic xRep x) = do
  App gRep yRep <- pure xRep
  HRefl <- eqTypeRep gRep (typeRep @m)
  pure (DynamicMFor yRep x)

{-
checkM :: forall m. Dynamic -> Maybe (DynamicMFor m)
checkM = \case
    Dynamic trMA a -> innerType (SomeTypeRep trMA) >>= \(SomeTypeRep (trA :: TypeRep @Type a)) ->
        pure $ DynamicMFor trA a
  where
    mConstr = typeRepTyCon (typeRep @(m ()))

innerType :: SomeTypeRep -> Maybe SomeTypeRep
innerType (SomeTypeRep x) = do
    let (con, args) = splitApps x
    if length args == 1
        then pure $ head args
        else Nothing
-}

asDyn :: DynamicM -> Dynamic
asDyn (DynamicM trA trM ma) = Dynamic (mkTrApp trM trA) ma

data DynamicMonad = DynamicMonad {
    -- | The bind operation of the monad applied to dynamic types.
    dynBind   :: DynamicM -> Dynamic -> Either TypeError DynamicM,
    -- | The return operation of the monad applied to dynamic types.
    dynReturn :: Dynamic -> DynamicM,
    -- | Helper function to check to see if an expression is in
    --    this monad.
    toDynM  :: Dynamic -> Either TypeError DynamicM
}

-- Equivalent to >>
dynSeq :: DynamicMonad -> DynamicM -> DynamicM -> Either TypeError DynamicM
dynSeq m x y = dynBind m x (dynApp (toDyn $ const @DynamicM @Dynamic) (toDyn y))

-- | Build up a DynamicMonad from a monad.
fromMonad :: forall m. (Monad m, Typeable m) => Proxy m -> DynamicMonad
fromMonad _ = DynamicMonad {
    dynReturn = \(Dynamic trA (v :: a)) ->
        DynamicM trA (typeRep @m) (return @m v),
    dynBind = \x f -> case x of
        (DynamicM trA trM1 (ma :: m1 a)) -> case testEquality (typeRep @m) trM1 of
            Nothing -> Left "Different types of monads used."
            Just Refl ->
              let res = do
                      let res1 = dynApp (toDyn (bindDyn @m @Dynamic @Dynamic))
                                (toDyn $ Dynamic trA <$> ma)
                      pure $ dynApp res1 f
              in case res of
                Nothing -> Left "Could not apply bind."
                Just dy -> toDynM dy,
    toDynM = toDynM
}
    where
      toDynM :: Dynamic -> Either TypeError DynamicM
      toDynM x = maybe (Left "Type error") Right $
             toDynamicM @m <$> checkM x

extractM :: DynamicMFor m -> m Dynamic
extractM (DynamicMFor pA ma) = Dynamic pA <$> ma

bindDyn :: forall m a b. (Monad m, Typeable m) => m a -> (a -> m Dynamic) -> m Dynamic
bindDyn = (>>=)

addDef :: InterpreterData -> String -> Dynamic -> InterpreterData
addDef ctx name def = ctx {
    exprDefs = exprDefs ctx <> singleton name def
}

interpret :: InterpreterData -> Ast -> Either TypeError Dynamic
interpret ctx@InterpreterData {..} = \case
    Atom x  -> maybe (Left "Could not find atom") Right $
        lookup x (exprDefs <> fmap snd operatorDefs)
    Ast.App f' x' -> do
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
    Lambda vars exp -> undefined
    Var x        -> Left $ "Found unbound variable: " ++ show x

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
        asDyn <$> interpretMonadicSequence ctx m (x:xs)

tryInferMonad :: InterpreterData -> SequenceExpr -> Either TypeError DynamicMonad
tryInferMonad ctx@InterpreterData {..} = \case
  Expr ast -> do
      res <- interpret ctx ast
      defs <- sequence <$> forM monadDefs (\def ->
          case toDynM def res of
              Left s -> pure Nothing
              Right dm -> pure $ Just def)
      case defs of
          Nothing  -> Left "Could not find matching monad."
          Just dms -> return $ head dms

  BindExpr s ast -> tryInferMonad ctx (Expr ast)

interpretMonadicSequence :: InterpreterData -> DynamicMonad -> [SequenceExpr] -> Either TypeError DynamicM
interpretMonadicSequence ctx@InterpreterData {..} m@DynamicMonad {..} = \case
    -- TODO: This probably won't work for monadic sequences returning a value.
    []     -> pure $ dynReturn (toDyn ())
    ((Expr x):xs) -> do
        x' <- interpret ctx x
        rest <- interpretMonadicSequence ctx m xs
        mx <- toDynM x'
        dynSeq m mx rest
    ((BindExpr x y):xs) -> undefined

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