{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Map.Bounded where

import qualified Data.Map.Strict as M

import           Data.Functor    ((<&>))
import           GHC.Generics    (Generic)
import           GHC.Stack       (HasCallStack)


data ValueAt v =
    Absent !Bool -- ^ Whether an insert will be honoured or ignored
  | Present !v
  deriving (Eq, Show, Read, Generic)

ifNonEmpty :: M.Map k v -> Maybe (M.Map k v)
ifNonEmpty m = if M.size m > 0 then Just m else Nothing

newValToMaybe :: ValueAt a -> Maybe a
newValToMaybe (Present v) = Just v
newValToMaybe (Absent  _) = Nothing

retain :: ValueAt v -> ValueAt v
retain = \case
  Present v -> Present v
  Absent  b -> Absent b

runAlterF
  :: Functor f => Bool -> (ValueAt v -> f (ValueAt v)) -> Maybe v -> f (Maybe v)
runAlterF canInsert f = \case
  Just v  -> newValToMaybe <$> f (Present v)
  Nothing -> if canInsert
    then Nothing <$ f (Absent False)
    else newValToMaybe <$> f (Absent True)

unsafeRunAlter
  :: (HasCallStack, Functor f)
  => (Maybe v -> f (Maybe v))
  -> ValueAt v
  -> f (ValueAt v)
unsafeRunAlter alter = \case
  Present v     -> maybeToNewVal <$> alter (Just v)
  Absent  False -> alter Nothing <&> \case
    Just v  -> error "tried to insert on Absent False"
    Nothing -> Absent False
  Absent True -> maybeToNewVal <$> alter Nothing
 where
  maybeToNewVal :: Maybe a -> ValueAt a
  maybeToNewVal (Just v) = Present v
  maybeToNewVal Nothing  = Absent True


adjustF_
  :: (Applicative f, Ord k) => (a -> f a) -> k -> M.Map k a -> f (M.Map k a)
adjustF_ f k m = case M.lookup k m of
  Just v  -> f v <&> \v' -> M.insert k v' m
  Nothing -> pure m


data BMap k v = BMap
  { bMap     :: !(M.Map k v)
  , bMaxSize :: !Int
  }
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)

newBMap :: Ord k => Int -> BMap k v
newBMap = BMap mempty

sizeBMap1 :: BMap k1 v -> Int
sizeBMap1 m = M.size (bMap m)

setMap1 :: BMap k v -> M.Map k' v' -> BMap k' v'
setMap1 m m' = m { bMap = m' }

adjustF
  :: (Applicative f, Ord k) => (a -> f a) -> k -> BMap k a -> f (BMap k a)
adjustF f k m = adjustF_ f k (bMap m) <&> setMap1 m

alterF
  :: (Functor f, Ord k)
  => (ValueAt v -> f (ValueAt v))
  -> k
  -> BMap k v
  -> f (BMap k v)
alterF f k m = M.alterF (f' (M.size bMap)) k bMap <&> setMap1 m
 where
  BMap {..} = m
  f' sz = runAlterF (sz >= bMaxSize) f

delete :: Ord k => k -> BMap k a -> BMap k a
delete k m = setMap1 m $ M.delete k $ bMap m

traverseWithKey :: Applicative t => (k -> a -> t b) -> BMap k a -> t (BMap k b)
traverseWithKey f m = setMap1 m <$> M.traverseWithKey f (bMap m)


data BMap2 k1 k2 v = BMap2
  { b2Map      :: !(M.Map k1 (M.Map k2 v))
  , b2MaxSize1 :: !Int
  -- ^ max number of k1 entries
  , b2MaxSize2 :: !Int
  -- ^ max number of k2 entries per k1
  }
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)

newBMap2 :: (Ord k1) => Int -> Int -> BMap2 k1 k2 v
newBMap2 = BMap2 mempty

sizeBMap2 :: BMap2 k1 k2 v -> Int
sizeBMap2 m = sum (M.size <$> b2Map m)

setMap2 :: BMap2 k1 k2 v -> M.Map k1' (M.Map k2' v') -> BMap2 k1' k2' v'
setMap2 m m' = m { b2Map = m' }

adjustF2
  :: (Applicative f, Ord k1, Ord k2)
  => (a -> f a)
  -> (k1, k2)
  -> BMap2 k1 k2 a
  -> f (BMap2 k1 k2 a)
adjustF2 f (k1, k2) m = adjustF_ (adjustF_ f k2) k1 (b2Map m) <&> setMap2 m

alterF2
  :: (Functor f, Ord k1, Ord k2)
  => (ValueAt v -> f (ValueAt v))
  -> (k1, k2)
  -> BMap2 k1 k2 v
  -> f (BMap2 k1 k2 v)
alterF2 f (k1, k2) m = M.alterF (alterF' (M.size b2Map)) k1 b2Map <&> setMap2 m
 where
  BMap2 {..} = m
  alterF' _  (Just mb) = ifNonEmpty <$> M.alterF (f' (M.size mb)) k2 mb
  alterF' sz Nothing   = if sz >= b2MaxSize1
    then Nothing <$ f (Absent False)
    else ifNonEmpty <$> M.alterF (f' 0) k2 mempty
  f' sz = runAlterF (sz >= b2MaxSize2) f

delete2 :: (Ord k1, Ord k2) => (k1, k2) -> BMap2 k1 k2 a -> BMap2 k1 k2 a
delete2 (k1, k2) m = setMap2 m $ M.alter alterF' k1 (b2Map m)
 where
  alterF' (Just mb) = ifNonEmpty $ M.delete k2 mb
  alterF' Nothing   = Nothing

traverseWithKey2
  :: Applicative t
  => ((k1, k2) -> a -> t b)
  -> BMap2 k1 k2 a
  -> t (BMap2 k1 k2 b)
traverseWithKey2 f m =
  setMap2 m <$> M.traverseWithKey (M.traverseWithKey . curry f) (b2Map m)
