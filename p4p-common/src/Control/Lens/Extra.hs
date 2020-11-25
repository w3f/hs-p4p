{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Lens.Extra where

import           Control.Lens               (ALens', Lens', LensLike, Over,
                                             unsafeSingular, use, (.=), (.~))
import           Control.Lens.At            (Index, IxValue)
import           Control.Lens.Indexed       (FoldableWithIndex (..),
                                             FunctorWithIndex (..),
                                             TraversableWithIndex (..))
import           Control.Lens.Strict        (Ixed (..))
import           Control.Lens.Unsound       (lensProduct)
import           Control.Monad.State.Strict (MonadState)
import           Data.Functor               ((<&>))
import           Data.Functor.Const         (Const (..))
import           Data.Functor.Identity      (Identity (..))
import           Data.Map.Bounded           as BM
import           GHC.Stack                  (HasCallStack)


-- https://github.com/ekmett/lens/issues/895
matching' :: LensLike (Either a) s t a b -> s -> Either t a
matching' k = either Right Left . k Left
{-# INLINE matching' #-}

-- | Slightly more powerful version of %%= that can run actions in the same
-- MonadState. Any changes to the part targeted by the input lens are
-- overwritten by the final result of the action.
(%%=!) :: MonadState s m => Lens' s a -> (a -> m (r, a)) -> m r
(%%=!) l f = do
  l0      <- use l
  (r, l1) <- f l0
  l .= l1
  pure r
infix 4 %%=!

-- | More general form of 'lensProduct', needed in certain situations.
lensProduct'
  :: Functor f
  => (forall g . Functor g => (a1 -> g b1) -> (s -> g s))
  -> (forall g . Functor g => (a2 -> g b2) -> (s -> g s))
  -> ((a1, a2) -> f (b1, b2))
  -> (s -> f s)
lensProduct' l1 l2 ff olds = ff (a1, a2) <&> f
 where
  a1 = getConst (l1 Const olds)
  a2 = getConst (l2 Const olds)
  f (b1, b2) = runIdentity
    (l2 (\_ -> Identity b2) (runIdentity (l1 (\_ -> Identity b1) olds)))

(%&&&%)
  :: Functor f => ALens' s a -> ALens' s b -> ((a, b) -> f (a, b)) -> s -> f s
(%&&&%) = lensProduct
infixl 7 %&&&%


-- | Like "At m" but modification can throw errors.
class Ixed m => At_ m where
  -- | Like "at" but modification can throw a monadic error.
  at_ :: Index m -> Lens' m (ValueAt (IxValue m))

-- | Like "at_" but any attempts to insert on a `Absent False` are raised
-- as Haskell runtime errors.
unsafeAt_ :: (HasCallStack, At_ m) => Index m -> Lens' m (Maybe (IxValue m))
unsafeAt_ k = at_ k . unsafeRunAlter
{-# INLINE unsafeAt_ #-}

sans_ :: (HasCallStack, At_ m) => Index m -> m -> m
sans_ k = unsafeAt_ k .~ Nothing
{-# INLINE sans_ #-}

-- | When setting an ix lens, if the key doesn't exist then that will silently
-- do nothing. This lens instead will raise a Haskell error. Depending on the
-- situation (e.g. when you are "sure" that the key must exist in all cases),
-- this may be preferable to silently doing nothing.
unsafeIx
  :: (HasCallStack, Functor f, Ixed t)
  => Index t
  -> Over (->) f t t (IxValue t) (IxValue t)
unsafeIx = unsafeSingular . ix
{-# INLINE unsafeIx #-}

instance FunctorWithIndex k (BMap k)
instance FoldableWithIndex k (BMap k)
instance TraversableWithIndex k (BMap k) where
  itraverse = BM.traverseWithKey
type instance Index (BMap k a) = k
type instance IxValue (BMap k a) = a
instance Ord k => Ixed (BMap k a) where
  ix k f = BM.adjustF f k
  {-# INLINE ix #-}
instance Ord k => At_ (BMap k a) where
  at_ k f = BM.alterF f k
  {-# INLINE at_ #-}

instance FunctorWithIndex (k1, k2) (BMap2 k1 k2)
instance FoldableWithIndex (k1, k2) (BMap2 k1 k2)
instance TraversableWithIndex (k1, k2) (BMap2 k1 k2) where
  itraverse = BM.traverseWithKey2
type instance Index (BMap2 k1 k2 a) = (k1, k2)
type instance IxValue (BMap2 k1 k2 a) = a
instance (Ord k1, Ord k2) => Ixed (BMap2 k1 k2 a) where
  ix k f = BM.adjustF2 f k
  {-# INLINE ix #-}
instance (Ord k1, Ord k2) => At_ (BMap2 k1 k2 a) where
  at_ k f = BM.alterF2 f k
  {-# INLINE at_ #-}
