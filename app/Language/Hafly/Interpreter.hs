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
import Data.List.NonEmpty (NonEmpty ((:|)))

data InterpreterData = InterpreterData {
    exprDefs     :: Map String Dynamic,
    operatorDefs :: Map String (Int, Dynamic),
    monadDefs    :: [SomeDynamicMonad]
}

data DynamicM = forall a m. (Monad m, Typeable m) => DynamicM (TypeRep a) (TypeRep m) (m a)

data DynamicMFor m = forall a. (Monad m, Typeable m) => DynamicMFor (TypeRep a) (m a)

toDynamicM :: forall m. (Monad m, Typeable m) => DynamicMFor m -> DynamicM
toDynamicM (DynamicMFor tr ma) = DynamicM tr (typeRep @m) ma

toDynM :: forall m. (Monad m, Typeable m) => Dynamic -> Maybe (m Dynamic)
toDynM (Dynamic xRep x) = do
  App gRep yRep <- pure xRep
  HRefl <- eqTypeRep gRep (typeRep @m)
  pure (Dynamic yRep <$> x)

checkM :: forall m. (Monad m, Typeable m) => Dynamic -> Maybe (DynamicMFor m)
checkM (Dynamic xRep x) = do
  App gRep yRep <- pure xRep
  HRefl <- eqTypeRep gRep (typeRep @m)
  pure (DynamicMFor yRep x)

asDyn :: DynamicM -> Dynamic
asDyn (DynamicM trA trM ma) = Dynamic (mkTrApp trM trA) ma

data DynamicMonad m = DynamicMonad {
    -- | The bind operation of the monad applied to dynamic types.
    dynBind   :: m Dynamic -> (Dynamic -> m Dynamic) -> m Dynamic,
    -- | The return operation of the monad applied to dynamic types.
    dynReturn :: Dynamic -> m Dynamic
}

data SomeDynamicMonad = forall m. (Monad m, Typeable m) => SomeDynamicMonad (DynamicMonad m)

-- Equivalent to >>
dynSeq :: DynamicMonad m -> m Dynamic -> m Dynamic -> m Dynamic
dynSeq m x y = dynBind m x (const y)

-- | Build up a DynamicMonad from a monad.
fromMonad :: forall m. (Monad m, Typeable m) => Proxy m -> SomeDynamicMonad
fromMonad _ = SomeDynamicMonad $ DynamicMonad {
    dynReturn = return,
    dynBind = (>>=) @m @Dynamic @Dynamic
}

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
    Lambda vars exp -> interpretLambda ctx vars exp
    Var x        -> Left $ "Found unbound variable: " ++ show x

interpretLambda :: InterpreterData -> [String] -> Ast -> Either TypeError Dynamic
interpretLambda ctx@InterpreterData {..} [] body =
    pure $ toDyn $ fromRight $ interpret ctx body
interpretLambda ctx@InterpreterData {..} (x:xs) body =
    interpretMultiArgLambda ctx body (x :| xs)

interpretMultiArgLambda :: InterpreterData -> Ast -> NonEmpty String -> Either TypeError Dynamic
interpretMultiArgLambda ctx@InterpreterData {..} body = \case
    (v :| []) ->
        pure $ toDyn $ \x -> fromRight $ interpret ctx $ subst v x body
    (v :| v' : vs) ->
        pure $ toDyn $ \x -> fromRight $ interpretMultiArgLambda ctx 
            (subst v x body) (v' :| vs)

-- TODO: Just a workaround for now to defer
-- evaluation inside of lambdas
fromRight (Right x) = x

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
        SomeDynamicMonad m <- tryInferMonad ctx x
        toDyn <$> interpretMonadicSequence ctx m (x:xs)

tryInferMonad :: InterpreterData -> SequenceExpr -> Either TypeError SomeDynamicMonad
tryInferMonad ctx@InterpreterData {..} = \case
  Expr ast -> do
      res <- interpret ctx ast
      defs <- sequence <$> forM monadDefs (\def@(SomeDynamicMonad (mnd :: DynamicMonad m)) ->
          case checkM @m res of
              Nothing -> pure Nothing
              Just dm -> pure $ Just def)
      case defs of
          Nothing  -> Left "Could not find matching monad."
          Just dms -> return $ head dms

  BindExpr s ast -> tryInferMonad ctx (Expr ast)

interpretMonadicSequence :: (Typeable m, Monad m) => InterpreterData -> DynamicMonad m -> [SequenceExpr] -> Either TypeError (m Dynamic)
interpretMonadicSequence ctx@InterpreterData {..} m@DynamicMonad {..} = \case
    -- TODO: This probably won't work for monadic sequences returning a value.
    []     -> pure $ dynReturn (toDyn ())
    ((Expr x):xs) -> do
        x' <- interpret ctx x
        rest <- interpretMonadicSequence ctx m xs
        mx <- maybe (Left "Expression was not of the correct monadic type") Right $ toDynM x'
        pure $ dynSeq m mx rest
    ((BindExpr x y):xs) -> undefined

-- | Interpret a Hafly expression in the IO monad.
interpretIO :: InterpreterData -> Ast -> IO ()
interpretIO ctx ast = do
    let result = interpret ctx ast
    case result of
        Left err -> putStrLn err
        Right x ->
            case fromDynamic @(IO Dynamic) x of
                Nothing -> putStrLn "Error: Expression was not of type IO ()"
                Just action -> action >> pure ()