{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-prof-auto #-} -- it space-leaks

module Control.Lens.Mutable.Extra
  ( Const(..)
  , SNat
  , sing
  , FakeAlloc1(..)
  , newFakeAlloc1
  , FakeAlloc2(..)
  , newFakeAlloc2
  )
where

-- external
import           Control.Lens             (_1, _2, _Wrapped)
import           Control.Lens.Mutable
import           Control.Lens.TH          (makeWrapped)
import           Data.Functor.Const       (Const (..))
import           Data.Singletons          (sing)
import           Data.Singletons.TypeLits (SNat)


uninitialised :: a
uninitialised = error "uninitialised memory"


newtype FakeAlloc1 a = FakeAlloc1 a
makeWrapped ''FakeAlloc1

newFakeAlloc1 :: FakeAlloc1 a
newFakeAlloc1 = FakeAlloc1 uninitialised

instance AsLens (FakeAlloc1 a) a (Const ()) where
  asLens _ = _Wrapped

instance Allocable (FakeAlloc1 a) a (Const ()) where
  alloc v1 (FakeAlloc1 _) = (Const (), FakeAlloc1 v1)
  isValid _ _ = error "programmer error, tried to isValid a FakeAlloc"
  free (Const _) (FakeAlloc1 v1) = (v1, FakeAlloc1 v1)


newtype FakeAlloc2 a b = FakeAlloc2 (a, b)
makeWrapped ''FakeAlloc2

newFakeAlloc2 :: FakeAlloc2 a b
newFakeAlloc2 = FakeAlloc2 (uninitialised, uninitialised)

instance AsLens (FakeAlloc2 a b) a (Const (SNat 1)) where
  asLens _ = _Wrapped . _1

instance Allocable (FakeAlloc2 a b) a (Const (SNat 1)) where
  alloc v1 (FakeAlloc2 (_, v2)) = (Const sing, FakeAlloc2 (v1, v2))
  isValid _ _ = error "programmer error, tried to isValid a FakeAlloc"
  free (Const _) (FakeAlloc2 (v1, v2)) = (v1, FakeAlloc2 (v1, v2))

instance AsLens (FakeAlloc2 a b) b (Const (SNat 2)) where
  asLens _ = _Wrapped . _2

instance Allocable (FakeAlloc2 a b) b (Const (SNat 2)) where
  alloc v2 (FakeAlloc2 (v1, _)) = (Const sing, FakeAlloc2 (v1, v2))
  isValid _ _ = error "programmer error, tried to isValid a FakeAlloc"
  free (Const _) (FakeAlloc2 (v1, v2)) = (v2, FakeAlloc2 (v1, v2))
