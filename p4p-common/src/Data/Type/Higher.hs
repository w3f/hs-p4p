{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Type.Higher where

-- external
import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.GADT.Compare         (GCompare (..), GEq (..),
                                            GOrdering (..), defaultCompare,
                                            defaultEq)
import           Data.GADT.Show            (GRead (..), GShow (..))
import           Data.Kind                 (Type)
import           Data.Proxy                (Proxy)
import           Data.Singletons           (Sing, SingI (..), withSingI)
import           Data.Some.Church          (Some (..))
import           Text.Show                 (showListWith)
import           Type.Reflection           (typeRep)


-- TODO: export to upstream dependent-sum
instance GShow Proxy where
  gshowsPrec = showsPrec

showCon :: String -> ShowS
showCon = showString

showArgG :: GShow t => t a -> ShowS
showArgG a = showChar ' ' . gshowsPrec 11 a

showArgS :: Show a => a -> ShowS
showArgS a = showChar ' ' . showsPrec 11 a

gshow1G :: GShow t => Int -> String -> t a -> ShowS
gshow1G d con a = showParen (d > 10) $ showCon con . showArgG a

gshow2SG :: (Show a, GShow t) => Int -> String -> a -> t b -> ShowS
gshow2SG d con a b = showParen (d > 10) $ showCon con . showArgS a . showArgG b

gshow2GG :: (GShow t1, GShow t2) => Int -> String -> t1 a -> t2 b -> ShowS
gshow2GG d con a b = showParen (d > 10) $ showCon con . showArgG a . showArgG b

gshow2LL :: (GShow t1, GShow t2) => Int -> String -> [t1 a] -> [t2 b] -> ShowS
gshow2LL d con a b = showParen (d > 10) $ do
  showCon con . showListWith showArgG a . showListWith showArgG b


-- | Generic instance wrapper.
--
-- This exists for convenience, and because defining e.g. GEq f => Eq (f t)
-- would cause overlapping instances everywhere and break the ecosystem.
--
-- TODO: export to upstream dependent-sum
newtype GWrap f t = GWrap { getWrapped :: f t }

instance GEq f => Eq (GWrap f t) where
  GWrap a == GWrap b = defaultEq a b

instance GCompare f => Ord (GWrap f t) where
  compare (GWrap a) (GWrap b) = defaultCompare a b


-- | Convenience wrappers to lift a kind so it takes extra '()' type param(s).
--
-- TODO: figure out how to use deriveGEq etc on a "L1 a"

data L1 p (a :: ()) where
  L1 :: !p -> L1 p '()

deriving instance Eq a => Eq (L1 a '())

instance Eq a => GEq (L1 a) where
  geq (L1 a0 :: L1 a s0) (L1 a1 :: L1 a s1) =
    case geq (typeRep @s0) (typeRep @s1) of
      Just r  -> if a0 == a1 then Just r else Nothing
      Nothing -> Nothing

deriving instance Ord a => Ord (L1 a '())

instance Ord a => GCompare (L1 a) where
  gcompare (L1 a0 :: L1 a s0) (L1 a1 :: L1 a s1) =
    case gcompare (typeRep @s0) (typeRep @s1) of
      GEQ -> case compare a0 a1 of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
      x -> x

deriving instance Show a => Show (L1 a x)

instance Show a => GShow (L1 a) where
  gshowsPrec = showsPrec

deriving instance Read a => Read (L1 a '())

instance Read a => GRead (L1 a) where
  greadsPrec d = readParen (d > 10) $ \s1 ->
    [ (S ($ L1 r), t) | ("L1", s2) <- lex s1, (r, t) <- readsPrec 11 s2 ]

_L1 :: Functor f => (a -> f b) -> L1 a '() -> f (L1 b '())
_L1 f (L1 x) = fmap L1 (f x)

deriveArgDict ''L1

data L2 p (a :: ((), ())) where
  L2 :: !p -> L2 p '( '(), '())

deriving instance Eq a => Eq (L2 a x)

instance Eq a => GEq (L2 a) where
  geq (L2 a0 :: L2 a s0) (L2 a1 :: L2 a s1) =
    case geq (typeRep @s0) (typeRep @s1) of
      Just r  -> if a0 == a1 then Just r else Nothing
      Nothing -> Nothing

deriving instance Ord a => Ord (L2 a x)

instance Ord a => GCompare (L2 a) where
  gcompare (L2 a0 :: L2 a s0) (L2 a1 :: L2 a s1) =
    case gcompare (typeRep @s0) (typeRep @s1) of
      GEQ -> case compare a0 a1 of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
      x -> x

deriving instance Show a => Show (L2 a x)

instance Show a => GShow (L2 a) where
  gshowsPrec = showsPrec

deriving instance Read a => Read (L2 a '( '(), '()))

instance Read a => GRead (L2 a) where
  greadsPrec d = readParen (d > 10) $ \s1 ->
    [ (S ($ L2 r), t) | ("L2", s2) <- lex s1, (r, t) <- readsPrec 11 s2 ]

_L2 :: Functor f => (a -> f b) -> L2 a '( '(), '()) -> f (L2 b '( '(), '()))
_L2 f (L2 x) = fmap L2 (f x)

deriveArgDict ''L2


-- | Convenience wrappers to unlift a kind so it hides the first type param(s).
data U1 (a :: k -> Type) where
  U1 :: SingI x => !(a x) -> U1 a

instance GEq a => Eq (U1 a) where
  (U1 a0) == (U1 a1) = defaultEq a0 a1

instance GCompare a => Ord (U1 a) where
  compare (U1 a0) (U1 a1) = defaultCompare a0 a1

-- | Reifies the quantified constraint @forall t. SingI (f t)@.
--
-- GHC can't infer this in the general case; this class provides an equivalent
-- substitute for it.
class SingIF (f :: k -> Type) where
  singF :: f t -> Sing t

instance SingIF (L1 f) where
  singF (L1 _) = sing

withSingIF :: SingIF f => f t -> (SingI t => a) -> a
withSingIF = withSingI . singF
