{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module P4P.Proc.Instances
  ( Lens'
  , SL(..)
  , MonadState(..)
  , MV(..)
  , PrimMonad(..)
  , newMutVar
  )
where

-- external
import           Control.Lens               (Lens', set, view)
import           Control.Monad.Primitive    (PrimMonad (..))
import           Control.Monad.State.Strict (MonadState (..), get, modify,
                                             state)
import           Data.Primitive.MutVar
import           Data.Tuple                 (swap)

-- internal
import           P4P.Proc.Internal


--------------------------------------------------------------------------------
-- Process over an immutable state.

newtype SL st s = SL (Lens' st s)

instance Proc s => Process (SL st s) where
  type State (SL st s) = s
  type Ctx (SL st s) m = (MonadState st m)
  proceed s = undefined -- FIXME: this requires a concept like MutableLensContext or something
  replace (SL lens) = modify . set lens
  suspend (SL lens) = view lens <$> get
  reactM (SL lens) = state . lens . react

--------------------------------------------------------------------------------
-- Process over a mutable state.

-- TODO: generalise using PrimST from Control.Monad.Primitive.Extra

newtype MV st s = MV (MutVar st s)

instance Proc s => Process (MV st s) where
  type State (MV st s) = s
  type Ctx (MV st s) m = (PrimMonad m, PrimState m ~ st)
  proceed = fmap MV <$> newMutVar
  replace (MV mv) = writeMutVar mv
  suspend (MV mv) = readMutVar mv
  reactM (MV mv) = atomicModifyMutVar' mv . fmap swap . react
