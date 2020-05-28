{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module P4P.Proc.Instances
  ( Lens'
  , PRef(..)
  , MonadState(..)
  , PMut(..)
  , PrimMonad(..)
  , PrimOpGroup(..)
  )
where

-- external
import           Control.Lens               (Lens', (%%=))
import           Control.Lens.Mutable       (Allocable (..), AsLens (..),
                                             IsoLST (..), MonadLST,
                                             PrimOpGroup (..), S (..))
import           Control.Monad.Primitive    (PrimMonad (..))
import           Control.Monad.State.Strict (MonadState (..), state)

-- internal
import           P4P.Proc.Internal


-- | Process over a pure reference.
newtype PRef ref st s = PRef (ref s)

instance (Allocable st s ref, Proc s) => Process (PRef ref st s) where
  type State (PRef ref st s) = s
  type Ctx (PRef ref st s) m = MonadState st m
  proceed s = fmap PRef $ state $ alloc s
  suspend (PRef r) = state $ free r

instance (Allocable st s ref, Proc s) => PureProcess (PRef ref st s) where
  runPure (PRef r) f = asLens r %%= f

-- | Process over an impure reference.
newtype PMut ref p st s = PMut (ref s)

instance (Allocable (S p st) s ref, Proc s) => Process (PMut ref p st s) where
  type State (PMut ref p st s) = s
  type Ctx (PMut ref p st s) m = MonadLST p st m
  proceed s = fmap PMut $ stToM $ alloc @(S p st) s
  suspend (PMut r) = stToM $ free @(S p st) r

instance (Allocable (S p st) s ref, Proc s) => PureProcess (PMut ref p st s) where
  runPure (PMut r) f = stToM $ asLens @(S p st) r f
