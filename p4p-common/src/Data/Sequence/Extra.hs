{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: just use lens instead of this hacky module
module Data.Sequence.Extra where

import           Data.Sequence
import           GHC.Generics  (Generic)
import           GHC.Stack     (HasCallStack)
import           Prelude       hiding (drop, filter, length, take, zipWith)

-- | Like @adjust'@ but the function can return something
state :: (a -> (v, a)) -> Int -> Seq a -> (Maybe v, Seq a)
state st i xs = case xs !? i of
  Nothing -> (Nothing, xs)
  Just x  -> let (!a, !x') = st x in (Just a, update i x' xs)

-- | Like @state@ but the index is a left-based predicate and the transition
-- function can either insert or remove an element.
statePL :: (Maybe a -> (v, Maybe a)) -> (a -> Bool) -> Seq a -> (v, Seq a)
statePL st predicate xs =
  let (hd, i, tl) = case breakl predicate xs of
        (hd', Empty     ) -> (hd', Nothing, Empty)
        (hd', i' :<| tl') -> (hd', Just i', tl')
      (!a, !o) = st i
  in  case o of
        Just o' -> (a, o' `seq` hd >< o' :<| tl)
        Nothing -> (a, hd >< tl)

-- | Insert if not already contained. O(n).
insertOr :: Eq a => a -> Seq a -> Seq a
insertOr v vs = case findIndexL (v ==) vs of
  Nothing -> vs |> v
  Just _  -> vs

-- | Bounded sequence.
--
-- Currently just a marker type to avoid forgetting to enforce the bound, does
-- not actually enforce the bound yet.
newtype BSeq a = BSeq { unBSeq :: Seq a }
 deriving (Eq, Ord, Show, Read, Generic, Functor, Applicative, Monad, Foldable, Traversable, Semigroup, Monoid)

newBSeq :: BSeq a
newBSeq = BSeq empty

lenBSeq :: BSeq a -> Int
lenBSeq (BSeq s) = length s

-- FIXME: take a bound parameter
bFromList :: [a] -> BSeq a
bFromList = BSeq . fromList

bLast :: HasCallStack => BSeq a -> a
bLast (BSeq (_ :|> t)) = t
bLast (BSeq Empty    ) = error "bLast of empty sequence!"

bDrop :: Int -> BSeq a -> BSeq a
bDrop i = BSeq . drop i . unBSeq

bTake :: Int -> BSeq a -> BSeq a
bTake i = BSeq . take i . unBSeq

bSortBy :: (a -> a -> Ordering) -> BSeq a -> BSeq a
bSortBy cmp = BSeq . sortBy cmp . unBSeq

bFilter :: (a -> Bool) -> BSeq a -> BSeq a
bFilter f = BSeq . filter f . unBSeq

bZipWith :: (a -> b -> c) -> BSeq a -> BSeq b -> BSeq c
bZipWith f a b = BSeq (zipWith f (unBSeq a) (unBSeq b))

bStatePL
  :: (a -> Bool)
  -> (BSeq a -> BSeq a)
  -> (Maybe a -> (v, Maybe a))
  -> BSeq a
  -> (v, BSeq a)
bStatePL p prune st = fmap (prune . BSeq) . statePL st p . unBSeq

-- | Prune from the front
bPruneL :: Int -> BSeq a -> BSeq a
bPruneL maxSz xs =
  if lenBSeq xs > maxSz then bDrop (lenBSeq xs - maxSz) xs else xs
