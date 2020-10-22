{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes        #-}

module Control.Lens.Strict where

-- external
import qualified Data.Map.Strict as M
import qualified Data.Vector     as V

import           Control.Lens    (Index, IxValue, Lens', LensLike', Traversal',
                                  (&), (.~), (<&>))


class Ixed m where
  ix :: Index m -> Traversal' m (IxValue m)
  default ix :: (Applicative f, At m) => Index m -> LensLike' f m (IxValue m)
  ix = ixAt
  {-# INLINE ix #-}

ixAt :: At m => Index m -> Traversal' m (IxValue m)
ixAt i = at i . traverse
{-# INLINE ixAt #-}

class Ixed m => At m where
  at :: Index m -> Lens' m (Maybe (IxValue m))

sans :: At m => Index m -> m -> m
sans k m = m & at k .~ Nothing
{-# INLINE sans #-}

instance Ixed (V.Vector a) where
  ix i f v
    | 0 <= i && i < V.length v = f (v V.! i) <&> \a -> a `seq` v V.// [(i, a)]
    | otherwise                = pure v
  {-# INLINE ix #-}

instance Ord k => Ixed (M.Map k a) where
  ix k f m = case M.lookup k m of
    Just v  -> f v <&> \v' -> v' `seq` M.insert k v' m
    Nothing -> pure m
  {-# INLINE ix #-}

instance Ord k => At (M.Map k a) where
  at k f = M.alterF f k
  {-# INLINE at #-}
