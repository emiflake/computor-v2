{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Computor.Type.Checker
  ( infer
  , runCheckerT
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Text as Text
import Data.Text (Text)

import Computor.Type
import Computor.Type.Matrix (Matrix(..))
import qualified Computor.Type.Matrix as Matrix
import Computor.Trans
import Computor.Error
import Computor.AST
import qualified Computor.Report.Tag as Tag
import Computor.AST.Identifier
import Computor.AST.Operator
import qualified Computor.Env as Env

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except

-- Temporary Substitution map
type Subst = Map Text Type

data TypeCheckerState
  = TypeCheckerState
  { freshVariable :: Int
  , substMap :: Subst
  }

  
newtype CheckerT m a =
  CheckerT
  { runCheckerT' :: StateT TypeCheckerState (ComputorT m) a
  }
  deriving
    ( Monad
    , Functor
    , Applicative
    , MonadError ComputorError
    , MonadState TypeCheckerState
    )

liftComputor :: Monad m => ComputorT m a -> CheckerT m a
liftComputor = CheckerT . lift 

runCheckerT :: CheckerT m a -> ComputorT m (a, TypeCheckerState)
runCheckerT =
  runCheckerT_ (TypeCheckerState 0 Map.empty)

runCheckerT_ :: TypeCheckerState -> CheckerT m a -> ComputorT m (a, TypeCheckerState)
runCheckerT_ initialState =
  (`runStateT` initialState) . runCheckerT'

fresh :: Monad m => CheckerT m Int
fresh =
  state
    (\s@TypeCheckerState{..} -> (freshVariable, s{ freshVariable = freshVariable + 1 }))

freshTyVar :: Monad m => CheckerT m Text
freshTyVar = do
  let easy = (Text.pack . (: [])) <$> ['a' .. 'z']
  n <- fresh
  -- TODO rewrite this part into neater code.
  pure $ case snd <$> find ((==n) . fst) (zip [0..] easy) of
    Nothing -> Text.pack ("t" <> show n)
    Just v -> v

applyS :: Subst -> Type -> Type
applyS _ t@(TyCon _) = t
applyS _ t@TyNumber = t
applyS s (TyForall f t) = TyForall f (applyS s t)
applyS s (TyMatrix m n t) = TyMatrix m n (applyS s t)
applyS s (a :-> b) = applyS s a :-> applyS s b
-- applyS s (TyApp f v) = TyApp (applyS s f) (applyS s v)
applyS s (TyVar v) = fromMaybe (TyVar v) $ Map.lookup v s

composeS :: Subst -> Subst -> Subst
composeS s f = f <> (applyS f <$> s)

flattenS :: [Subst] -> Subst
flattenS = foldr composeS Map.empty

-- -- Check whether type var occurs in type
occurs :: Text -> Type -> Bool
occurs v (TyVar v') = v == v'
occurs _ (TyCon _) = False
occurs _ TyNumber = False
occurs v (TyMatrix _ _ t) = occurs v t
occurs _ (TyForall _ _) = False -- iffy
-- occurs v (TyApp _ v') = occurs v v'
occurs v (a :-> b) = occurs v a || occurs v b

unify :: Monad m => Type -> Type -> CheckerT m Subst
unify t1 t2 | t1 == t2 = pure Map.empty
unify (TyVar v) tv@(TyVar _) = pure (Map.singleton v tv)
unify (TyVar v) t2
  | occurs v t2 = throwError . TypeError $ OccursCheck v t2
  | otherwise = pure (Map.singleton v t2)
unify t1 (TyVar v)
  | occurs v t1 = throwError . TypeError $ OccursCheck v t1
  | otherwise = pure (Map.singleton v t1)
unify (a :-> b) (c :-> d) = do
  s1 <- unify a c
  s2 <- unify (applyS s1 b) (applyS s1 d)
  pure $ s1 `composeS` s2
-- unify (TyApp s v) (TyApp s' v') = do
--   s1 <- unify s s'
--   s2 <- unify (applyS s1 v) (applyS s1 v')
--   pure $ s1 `composeS` s2
unify t1 t2
  | TyNumber `elem` [t1, t2] && (t1 `elem` degradesToNumber || t2 `elem` degradesToNumber)
  = pure Map.empty
unify t1 t2 = throwError . TypeError $ UnificationError t1 t2

degradesToNumber :: [Type]
degradesToNumber =
  [ realTy
  , complexTy
  , rationalTy
  ]
instantiate :: Monad m => Type -> CheckerT m Type
instantiate (TyForall n ty) = do
  freshTy <- freshTyVar
  applyS (Map.singleton n (TyVar freshTy)) <$> instantiate ty
instantiate (a :-> b) = (:->) <$> instantiate a <*> instantiate b
instantiate t@(TyCon _) = pure t
instantiate t@TyNumber = pure t
instantiate (TyMatrix m n t) = TyMatrix m n <$> instantiate t
-- instantiate (TyApp f v) = TyApp <$> instantiate f <*> instantiate v
instantiate (TyVar tv) = pure (TyVar tv)

-- -- add forall to all free variables
generalize :: Type -> Type
generalize ty = foldr TyForall ty (ftv ty)

{- This function is responsible for ensuring your ascription is strictly unifiable in one direction, namely, you are able to restrict types, but not extend them.
   This function needs verification.
-}

ascCheck :: Monad m => Map Text Type -> Expr -> Type -> CheckerT m Type
ascCheck env expr t = do
  (s, t') <- infer' env expr
  ss <- unify t' t
  let ut = applyS (s `composeS` ss) t
  if ut == t
    then pure t
    else throwError . TypeError $ UnificationError t t'

infer :: Monad m => Expr -> CheckerT m (Subst, Type)
infer expr = do
  env <- liftComputor (Map.map fst . Env.getEnvironment <$> get)
  (s, ty) <- infer' env expr 
  pure (s, generalize ty)

infer' :: Monad m => Map Text Type -> Expr -> CheckerT m (Subst, Type)
infer' env expr = case expr of
  Tag.At s (LitIdent ident) ->
    case Map.lookup (unIdentifier ident) env of
      Nothing -> throwError . TypeError $ MissingVariable (unIdentifier ident)
      Just ty -> pure (Map.empty, ty)

  Tag.At _ (LitNum _) ->
    pure (Map.empty, realTy)

  Tag.At _ LitImag ->
    pure (Map.empty, complexTy)

  Tag.At _ (LitMatrix matrix@Matrix{..}) -> do
    freshTy <- freshTyVar
    subsL <- forM (Matrix.toList matrix) $ \elem -> do
      (s, ct) <- infer' env elem
      ss <- unify ct (TyVar freshTy)
      pure (s <> ss)
    let subs = flattenS subsL
    let t = generalize (applyS subs (TyVar freshTy))
    pure (subs, TyMatrix rows columns t)

  -- A.StringLit _ -> pure (Map.empty, Type.stringTy)
  -- A.Asc e t -> do
  --   rt <- ascCheck env e t
  --   pure (Map.empty, rt)
  -- A.If c t f -> do
  --   (cs, ct) <- infer env c
  --   (ts, tt) <- infer env t
  --   (fs, ft) <- infer env f
  --   tvar <- freshTyVar
  --   bs <- unify ct Type.boolTy
  --   ct' <- instantiate $ applyS bs ct
  --   tt' <- instantiate $ applyS (cs `composeS` bs) tt
  --   ft' <- instantiate $ applyS (foldr composeS Map.empty [ts, cs, bs]) ft
  --   ss <- unify (Type.boolTy :~> TyVar tvar :~> TyVar tvar) (ct' :~> tt' :~> ft')
  --   let sss = flattenS [ss, fs, ts, cs, bs]
  --   pure (sss, applyS sss (TyVar tvar))
  Tag.At _ (BinOp op l r) -> do
    opTy <- inferOp op
    (ls, lt) <- infer' env l
    (rs, rt) <- infer' env r
    tvar <- freshTyVar
    lt' <- instantiate lt
    rt' <- instantiate rt
    ss <- unify (lt' :-> applyS ls rt' :-> TyVar tvar) opTy
    let sss = flattenS [ss, rs, ls]
    pure (sss, applyS sss (TyVar tvar))
  Tag.At _ (App fun arg) -> do
    tvar <- freshTyVar
    (fs, ft) <- infer' env fun
    (as, at) <- infer' env arg
    ft' <- instantiate ft
    at' <- instantiate (applyS fs at)
    ss <- unify (at' :-> TyVar tvar) ft'
    let sss = flattenS [fs, as, ss]
    pure (sss, applyS sss (TyVar tvar))
  Tag.At _ (Lam param body) -> do
    tvar <- freshTyVar
    (bs, bt) <- infer' (Map.insert (unIdentifier param) (TyVar tvar) env) body
    bt' <- instantiate bt
    pure (bs, applyS bs (TyVar tvar) :-> bt')
  -- A.Let name rhs body -> do
  --   tvar <- freshTyVar
  --   (rs, rt) <- infer (Map.insert name (TyVar tvar) env) rhs
  --   rss <- unify (TyVar tvar) rt
  --   let rt' = generalizeIfValue rhs (applyS rss (TyVar tvar))
  --   (bs, bt) <- infer (Map.insert name rt' env) body
  --   pure (bs `composeS` rs, applyS rs bt)
  where
    inferOp Add = pure (TyNumber :-> TyNumber :-> TyNumber)
    inferOp Subtract = pure (TyNumber :-> TyNumber :-> TyNumber)
    inferOp Multiply = pure (TyNumber :-> TyNumber :-> TyNumber)
    inferOp Divide = pure (TyNumber :-> TyNumber :-> TyNumber)
    inferOp Modulus = pure (TyNumber :-> TyNumber :-> TyNumber)
    inferOp Power = pure (TyNumber :-> TyNumber :-> TyNumber)
    inferOp Compose = do
      freshTy <- freshTyVar
      pure (TyVar freshTy :-> TyVar freshTy :-> TyVar freshTy)

-- generalizeIfValue :: A.Expr -> Type -> Type
-- generalizeIfValue e ty
--   | isValue e = generalize ty
--   | otherwise = ty

-- isValue :: A.Expr -> Bool
-- isValue expr' = case expr' of
--   (A.Let _ r b) -> isValue r && isValue b
--   (A.Lam _ _) -> True
--   (A.App _ _) -> False
--   A.BinOp {} -> False
--   A.If {} -> False
--   (A.Asc e _) -> isValue e
--   (A.NumLit _) -> True
--   (A.StringLit _) -> True
--   (A.Var _) -> False
