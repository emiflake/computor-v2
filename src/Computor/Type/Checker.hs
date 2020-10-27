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
applyS s st@(Tag.At span t) =
  case t of
    TyNumber -> Tag.At span t
    TyForall f t -> Tag.At span $ TyForall f (applyS s t)
    TyMatrix m n t -> Tag.At span $ TyMatrix m n (applyS s t)
    a :-> b -> Tag.At span $ applyS s a :-> applyS s b
    TyVar v -> fromMaybe (Tag.At span $ TyVar v) $ Map.lookup v s
    _ -> st

composeS :: Subst -> Subst -> Subst
composeS s f = f <> (applyS f <$> s)

flattenS :: [Subst] -> Subst
flattenS = foldr composeS Map.empty

-- -- Check whether type var occurs in type
occurs :: Text -> Type -> Bool
occurs v (Tag.At s t) = case t of
  TyVar v' -> v == v'
  TyCon _ -> False
  TyNumber -> False
  TyMatrix _ _ st -> occurs v st
  TyForall _ _ -> False -- iffy
  a :-> b -> occurs v a || occurs v b

unify :: Monad m => Type -> Type -> CheckerT m Subst
unify st1@(Tag.At s1 t1) st2@(Tag.At s2 t2) =
  case (t1, t2) of
    (t1, t2) | t1 == t2 -> pure Map.empty
    (TyVar v, tv@(TyVar _)) -> pure (Map.singleton v st2)
    (TyVar v, t2)
      | occurs v st2 -> throwError . TypeError $ OccursCheck v st2
      | otherwise -> pure (Map.singleton v st2)
    (t1, TyVar v)
      | occurs v st1 -> throwError . TypeError $ OccursCheck v st1
      | otherwise -> pure (Map.singleton v st1)
    (a :-> b, c :-> d) -> do
      s1 <- unify a c
      s2 <- unify (applyS s1 b) (applyS s1 d)
      pure $ s1 `composeS` s2
-- unify (TyApp s v) (TyApp s' v') = do
--   s1 <- unify s s'
--   s2 <- unify (applyS s1 v) (applyS s1 v')
--   pure $ s1 `composeS` s2
    (t1, t2)
      | TyNumber `elem` [t1, t2] && (t1 `elem` degradesToNumber || t2 `elem` degradesToNumber)
      -> pure Map.empty
    (t1, t2) -> throwError . TypeError $ UnificationError st1 st2

degradesToNumber :: [Type']
degradesToNumber =
  [ realTy
  , complexTy
  , rationalTy
  ]

instantiate :: Monad m => Type -> CheckerT m Type
instantiate st@(Tag.At s t) =
  case t of
    (TyForall n ty) -> do
      freshTy <- freshTyVar
      applyS (Map.singleton n (Tag.At s $ TyVar freshTy)) <$> instantiate ty
    (a :-> b) -> (\ia ib -> Tag.At s (ia :-> ib)) <$> instantiate a <*> instantiate b
    (TyCon _) -> pure st
    TyNumber -> pure st
    (TyMatrix m n t) -> Tag.At s . TyMatrix m n <$> instantiate t
    -- instantiate (TyApp f v) = TyApp <$> instantiate f <*> instantiate v
    (TyVar tv) -> pure st

-- -- add forall to all free variables
generalize :: Type -> Type
generalize st@(Tag.At s t) = foldr (\fv acc -> Tag.At s (TyForall fv acc)) st (ftv st)

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
infer' env (Tag.At span expr) = case expr of
  (LitIdent ident) ->
    case Map.lookup (unIdentifier ident) env of
      Nothing -> throwError . TypeError $ MissingVariable span (unIdentifier ident)
      Just ty -> pure (Map.empty, ty)

  (LitNum _) ->
    pure (Map.empty, Tag.At span realTy)

  LitImag ->
    pure (Map.empty, Tag.At span complexTy)

  (LitMatrix matrix@Matrix{..}) -> do
    freshTy <- freshTyVar
    subsL <- forM (Matrix.toList matrix) $ \elem -> do
      (s, ct) <- infer' env elem
      ss <- unify ct (Tag.At span (TyVar freshTy))
      pure (s <> ss)
    let subs = flattenS subsL
    let t = generalize (applyS subs (Tag.At span (TyVar freshTy)))
    pure (subs, Tag.At span $ TyMatrix rows columns t)

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
  (BinOp op l@(Tag.At sl _) r@(Tag.At sr _)) -> do
    opTy <- inferOp span op
    (ls, lt) <- infer' env l
    (rs, rt) <- infer' env r
    tvar <- Tag.At sr . TyVar <$> freshTyVar
    lt' <- instantiate lt
    rt' <- instantiate rt
    ss <- unify (Tag.At span $ lt' :-> (Tag.At sr (applyS ls rt' :-> tvar))) opTy
    let sss = flattenS [ss, rs, ls]
    pure (sss, applyS sss tvar)
  (App fun@(Tag.At sfun _) arg@(Tag.At sarg _)) -> do
    tvar <- Tag.At span . TyVar <$> freshTyVar
    (fs, ft) <- infer' env fun
    (as, at) <- infer' env arg
    ft' <- instantiate ft
    at' <- instantiate (applyS fs at)
    ss <- unify (Tag.At sfun $ at' :-> tvar) ft'
    let sss = flattenS [fs, as, ss]
    pure (sss, applyS sss tvar)
  (Lam param body) -> do
    tvar <- Tag.At span . TyVar <$> freshTyVar
    (bs, bt) <- infer' (Map.insert (unIdentifier param) tvar env) body
    bt' <- instantiate bt
    pure (bs, Tag.At span $ applyS bs tvar :-> bt')
  -- A.Let name rhs body -> do
  --   tvar <- freshTyVar
  --   (rs, rt) <- infer (Map.insert name (TyVar tvar) env) rhs
  --   rss <- unify (TyVar tvar) rt
  --   let rt' = generalizeIfValue rhs (applyS rss (TyVar tvar))
  --   (bs, bt) <- infer (Map.insert name rt' env) body
  --   pure (bs `composeS` rs, applyS rs bt)
  where
    constructBinopSpanned s a b c =
      Tag.At s (Tag.At s a :-> (Tag.At s (Tag.At s b :-> Tag.At s c)))

    inferOp s Add = pure $ constructBinopSpanned s TyNumber TyNumber TyNumber
    inferOp s Subtract = pure $ constructBinopSpanned s TyNumber TyNumber TyNumber
    inferOp s Multiply = pure $ constructBinopSpanned s TyNumber TyNumber TyNumber
    inferOp s Divide = pure $ constructBinopSpanned s TyNumber TyNumber TyNumber
    inferOp s Modulus = pure $ constructBinopSpanned s TyNumber TyNumber TyNumber
    inferOp s Power = pure $ constructBinopSpanned s TyNumber TyNumber TyNumber
    inferOp s Compose = do
      a <- Tag.At s . TyVar <$> freshTyVar
      b <- Tag.At s . TyVar <$> freshTyVar
      c <- Tag.At s . TyVar <$> freshTyVar
      pure $ constructBinopSpanned s (b :-> c) (a :-> b) (a :-> c)



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
