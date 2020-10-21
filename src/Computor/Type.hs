{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Computor.Type
  ( Type(..)
  , realTy
  , stringTy
  , unitTy
  , boolTy
  , rationalTy
  , complexTy
  , ftv
  )
  where

{-

# The premise of the type system.

Types are hard, especially when things need to 'just work'.

Having separate type for rational, complex, real and integer is complicated.
This is especially hard when it comes to functions that can viably work on many types together dynamically.

Let's consider addition `+`:
If our calculator is to be useful, we must be able to add a real number and a complex number.

let R be a real number, let C be a complex number broken down into (a + bi)

R + C = R + a + bi, clearly, this becomes a complex number too.

So we might say `+` : Real -> Complex -> Complex

But this is overly specific.

## Proposal:

Dynamic supertype 'Number', supertype of 'Rational', 'Real', 'Complex', 'Integer', and maybe even something else in the future. (Possibly matrices may *need* to be a subtype of Number)

All operations that make sense in more than one type will function in this domain.

`+` : Number -> Number -> Number

with the caveat that this may not always be safe at runtime.

In the future, you might imagine functions such as

unsafeToReal : Number -> Real
unsafeToRational : Number -> Rational
... etc

These are important to be defined, because otherwise, we will lose all information about types instantly.

Example hard to define function without having too much insight into the future:

unsafeSolve : (Real -> Number) -> ???

-}

import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.Set as Set
import Data.Set (Set)

import Prettyprinter


ftv :: Type -> Set Text
ftv ty = case ty of
  TyCon _ -> Set.empty
  TyNumber -> Set.empty
  TyMatrix _ _ t -> ftv t
  a :-> b -> Set.union (ftv a) (ftv b)
  TyVar v -> Set.singleton v
  -- TyApp _ _ -> Set.empty
  TyForall v t -> Set.delete v (ftv t)

data Type
  -- The number type
  -- A humongous dynamic type that does everything you'd ever want, but is unsafe cuz fuck me is it hard to make it safe
  = TyNumber

  -- A type variable that lives somewhere in unification scope.
  | TyVar Text

  -- A type *constructor*, which represents a concrete type, like `String`, for instance.
  | TyCon Text

  -- The type of a function between two types
  | Type :-> Type

  -- The type of a matrix, parameterized by its size, and its contained type.
  -- Who doesn't like matrices of strings. :)
  -- Perhaps we could use some interesting notation? Like String[3 * 3]
  -- TODO: what happens when we don't know the size?
  --       maybe it's not that good an idea to have these.
  --       needs exploration.
  | TyMatrix Int Int Type

  -- Parametrized polymorphism, a la Hindley Milner
  -- id : forall a. a -> a
  | TyForall Text Type
  deriving (Show, Eq)

infixr 9 :->

stringTy :: Type
stringTy = TyCon "String"

realTy :: Type
realTy = TyCon "Real"

rationalTy :: Type
rationalTy = TyCon "Rational"

complexTy :: Type
complexTy = TyCon "Complex"

boolTy :: Type
boolTy = TyCon "Bool"

unitTy :: Type
unitTy = TyCon "Unit"

instance Pretty Type where
  pretty = \case
    TyNumber -> "Number"
    TyVar name -> pretty name
    TyCon name -> pretty name
    from :-> to -> deepTy from <+> "->" <+> pretty to
    TyMatrix m n ty -> "Matrix" <+> pretty m <+> pretty n <+> deepTy ty
    TyForall binding ty -> "∀" <+> pretty binding <> "." <+> deepTy ty
    where
      deepTy ty =
        case ty of
          _ :-> _ -> "(" <> pretty ty <> ")"
          _ -> pretty ty
