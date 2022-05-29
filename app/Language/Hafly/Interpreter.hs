{-# LANGUAGE GADTs #-}

module Language.Hafly.Interpreter where
import Data.Dynamic
import Data.Data (Proxy)
import Language.Hafly.Ast hiding (App, Const)
import Type.Reflection hiding (App)
import Data.MultiMap hiding (filter)
import Prelude hiding (lookup)
import Data.Maybe
import Type.Reflection hiding (TypeRep, typeRep, typeRepTyCon)
import qualified Language.Hafly.Ast as Ast
import Data.Type.Equality
import Data.Typeable (funResultTy, typeRepArgs, gcast)
import Type.Reflection.Unsafe (mkTrApp)
import Unsafe.Coerce (unsafeCoerce)
import Data.Kind (Type)
import Control.Monad
import Control.Arrow
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Control.Monad.Combinators.Expr
import Text.Megaparsec hiding (empty)
import Data.Void
import Control.Applicative hiding (empty)
import Data.Foldable
import Data.List hiding (insert, lookup)
import Control.Monad.Fix

data InterpreterContext = InterpreterContext {
    exprDefs     :: MultiMap String Dynamic,
    operatorDefs :: [[Operator (Const (String, Dynamic)) Void]],
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

addDef :: InterpreterContext -> String -> Dynamic -> InterpreterContext
addDef ctx name def = ctx {
    exprDefs = fromMap $ toMap (exprDefs ctx) <> toMap (insert name def empty)
}

extractSpecs :: Operator (Const a) b -> a
extractSpecs = \case
  InfixN co  -> getConst co
  InfixL co  -> getConst co
  InfixR co  -> getConst co
  Prefix co  -> getConst co
  Postfix co -> getConst co
  TernR co   -> getConst co

interpretRec :: InterpreterContext -> String -> Ast -> Either TypeError Dynamic
interpretRec ctx name x = dynApp (toDyn $ fix @Dynamic) <$>
    interpret ctx (Lambda [name] x)

interpret :: InterpreterContext -> Ast -> Either TypeError Dynamic
interpret ctx@InterpreterContext {..} = \case
    Var x  -> dispatched x $
        lookup x $ fromMap (
            toMap exprDefs <>
            toMap (fromList (extractSpecs <$> join operatorDefs)))
    Ast.Const x -> pure x
    Ast.App f' x' -> do
        f <- interpret ctx f'
        x <- interpret ctx x'

        let argType = case x of
                Dynamic z _ -> SomeTypeRep z
        let functionType = case f of
                Dynamic z _ -> SomeTypeRep z

        flattenDyn <$>
            flexibleDynApp f x
    Sequence seq ->
        interpretSequence ctx seq
    Lambda vars exp ->
        interpretLambda ctx vars exp
    Record r -> toDyn <$>
        sequence (interpret ctx <$> r)
    List xs -> toDyn <$>
        sequence (interpret ctx <$> xs)
    Cond _if _then _else -> do
        condition <- asBool =<< interpret ctx _if
        if condition
            then interpret ctx _then
            else interpret ctx _else

asBool :: Dynamic -> Either TypeError Bool
asBool (Dynamic tr x) = case testEquality tr (typeRep @Bool) of
    Nothing   -> Left $ "Could not cast expression of type " ++ show tr ++ " to Bool."
    Just Refl -> Right x

dispatched :: String -> [Dynamic] -> Either TypeError Dynamic
dispatched x [] = Left $ "Found unbound variable " ++ x
dispatched _ [x] = Right x
dispatched _ (x:xs) = Right $
    -- TODO: Build a function that inspects the type
    -- of it's argument to figure out which function to
    -- execute.
    singlyDispatched (x : xs)

singlyDispatched :: [Dynamic] -> Dynamic
singlyDispatched fs = toDyn $ \(x :: Dynamic) ->
    case find (== dynTypeRep x) types of
        Nothing  -> error $ "Cannot apply dynamically dispatched function of types" ++
            show types ++ " to an argument of type: " ++ show (dynTypeRep x)
        Just t -> let
              indexOfType = fromJust $ elemIndex t types
              fn = fs !! indexOfType
          in dynApp fn x
    where
      types = mapMaybe (dynArgTypeRep . dynTypeRep) fs

dynArgTypeRep :: SomeTypeRep -> Maybe SomeTypeRep
dynArgTypeRep (SomeTypeRep x) = do
    Fun arg _ <- pure x
    pure $ SomeTypeRep arg

interpretLambda :: InterpreterContext -> [String] -> Ast -> Either TypeError Dynamic
interpretLambda ctx@InterpreterContext {..} [] body =
    pure $ toDyn $ fromRight $ interpret ctx body
interpretLambda ctx@InterpreterContext {..} (x:xs) body =
    interpretMultiArgLambda ctx body (x :| xs)

interpretMultiArgLambda :: InterpreterContext -> Ast -> NonEmpty String -> Either TypeError Dynamic
interpretMultiArgLambda ctx@InterpreterContext {..} body = \case
    (v :| []) ->
        pure $ toDyn $ \x -> fromRight $ interpret ctx $ subst v (Ast.Const x) body
    (v :| v' : vs) ->
        pure $ toDyn $ \x -> fromRight $ interpretMultiArgLambda ctx
            (subst v (Ast.Const x) body) (v' :| vs)

-- TODO: Just a workaround for now to defer
-- evaluation inside of lambdas
fromRight (Right x) = x
fromRight (Left err) = error err

interpretSequence :: InterpreterContext -> SequenceAst -> Either TypeError Dynamic
interpretSequence ctx@InterpreterContext {..} = \case
    SequenceAst [] -> Left "Cannot interpret an empty sequence"
    SequenceAst (x:xs) -> do
        SomeDynamicMonad m <- tryInferMonad ctx x
        toDyn <$> interpretMonadicSequence ctx m (x:xs)

tryInferMonad :: InterpreterContext -> SequenceExpr -> Either TypeError SomeDynamicMonad
tryInferMonad ctx@InterpreterContext {..} = \case
  Expr ast -> do
      res <- interpret ctx ast
      defs <- sequence <$> forM monadDefs (\def@(SomeDynamicMonad (mnd :: DynamicMonad m)) ->
          case checkM @m res of
              Nothing -> pure Nothing
              Just dm -> pure $ Just def)
      case defs of
          Just (d:dms) -> return d
          _ -> Left "Could not find matching monad."

  BindExpr s ast -> tryInferMonad ctx (Expr ast)

interpretMonadicSequence :: (Typeable m, Monad m) => InterpreterContext -> DynamicMonad m -> [SequenceExpr] -> Either TypeError (m Dynamic)
interpretMonadicSequence ctx@InterpreterContext {..} m@DynamicMonad {..} = \case
    -- TODO: This probably won't work for monadic sequences returning a value.
    []     -> pure $ dynReturn (toDyn ())
    ((Expr x):xs) -> do
        x' <- interpret ctx x
        rest <- interpretMonadicSequence ctx m xs
        mx <- maybe (Left "Expression was not of the correct monadic type") Right $ toDynM x'
        pure $ dynSeq m mx rest
    ((BindExpr x y):xs) -> undefined

-- | Interpret a Hafly expression in the IO monad.
interpretIO :: InterpreterContext -> Ast -> Maybe (IO ())
interpretIO ctx ast = do
    let result = interpret ctx ast
    case result of
        Left err -> Nothing
        Right x ->
            case fromDynamic @(IO Dynamic) x of
                Nothing -> fromDynamic @(IO ()) x
                Just action -> Just $ action >> pure ()

-- | Version of dynApply that will attempt to lift arguments to 
-- Dynamic if nescesary.
flexibleDynApp :: Dynamic -> Dynamic -> Either TypeError Dynamic
flexibleDynApp f x = do
    res <- maybe (Left $ "Cannot apply function of type " ++ show (dynTypeRep f) ++
            " to argument of type " ++ show (dynTypeRep x)) Right
        (sequence $ filter isJust [dynApply f x, dynApply f (toDyn x)])
    case res of
        [] -> Left $ "Cannot apply function of type " ++ show (dynTypeRep f) ++ " to argument of type " ++ show (dynTypeRep x)
        (x:xs) -> Right x

flexibleDynApply :: Dynamic -> Dynamic -> Dynamic
flexibleDynApply f x = fromRight $ flexibleDynApp f x

-- | Helper function to flatten nested dynamics
-- into a single Dynamic.
flattenDyn :: Dynamic -> Dynamic
flattenDyn dyn@(Dynamic tr x) = case testEquality tr (typeRep @Dynamic) of
  Nothing -> dyn
  Just Refl -> flattenDyn x
