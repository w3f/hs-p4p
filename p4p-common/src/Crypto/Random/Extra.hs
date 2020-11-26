{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Crypto.Random.Extra
  ( randomWord64Generate
  , randomDoubleGenerate
  , randomProbGenerate
  , generateUntil
  , DRG'(..)
  , initializeFrom
  , ChaChaDRG'
  , ChaChaDRGInsecure
  , module Data.ByteArray
  , module Crypto.Random
  ) where

import qualified Crypto.Cipher.ChaCha.Extra as C
import qualified Data.ByteArray             as B

import           Crypto.Random
import           Data.Bits                  (Bits (..))
import           Data.ByteArray             (ByteArray, ByteArrayAccess,
                                             ScrubbedBytes)
import           Data.Word                  (Word64)
import           Foreign.Storable           (Storable (..))
import           GHC.Float                  (castWord64ToDouble)
import           System.IO.Unsafe


randomWord64Generate :: DRG g => g -> (Word64, g)
randomWord64Generate g0 =
  let (b, g1) = randomBytesGenerate @_ @ScrubbedBytes 8 g0
  in  (unsafeDupablePerformIO $ B.withByteArray b peek, g1)

randomDoubleGenerate :: DRG g => g -> (Double, g)
randomDoubleGenerate g0 =
  let (w, g1) = randomWord64Generate g0 in (castWord64ToDouble w, g1)

-- | Generate a random value between [0, 1).
randomProbGenerate :: DRG g => g -> (Double, g)
randomProbGenerate g0 =
  let (w, g1) = randomWord64Generate g0
      w12     = (w .&. 0x000fffffffffffff .|. 0x3ff0000000000000) -- some double in [1, 2)
  in  (castWord64ToDouble w12 - 1.0, g1)

-- | Keep generating a value until it fits a predicate.
--
-- >>> let genNonZero g0 = generateUntil (/= 0.0) randomProbGenerate g0
generateUntil :: DRG g => (a -> Bool) -> (g -> (a, g)) -> g -> (a, g)
generateUntil match gen g0 =
  let (a, g1) = gen g0
  in  if match a then (a, g1) else generateUntil match gen g1


-- | Like 'DRG' but supports initialisation from some arbitrary seed.
class DRG gen => DRG' gen where
  initialize :: ByteArrayAccess seed => seed -> gen
  -- | Length of seed in bytes
  seedLength :: Int

initializeFrom
  :: forall gen f
   . (DRG' gen, Functor f)
  => (forall seed . ByteArray seed => Int -> f seed)
  -> f gen
initializeFrom getEntropy =
  initialize <$> getEntropy @ScrubbedBytes (seedLength @gen)


-- | ChaCha Deterministic Random Generator
type ChaChaDRG' = C.StatePure

instance DRG ChaChaDRG' where
  randomBytesGenerate n st =
    if n <= 0 then (B.empty, st) else C.generatePure st n

instance DRG' ChaChaDRG' where
  initialize = C.initializePure
  seedLength = 40

-- | ChaCha Deterministic Random Generator
--
-- This version has an extra Read instance which is useful for simulations, but
-- is insecure for real-world use.
type ChaChaDRGInsecure = C.StatePureInsecure

instance DRG ChaChaDRGInsecure where
  randomBytesGenerate n st =
    if n <= 0 then (B.empty, st) else C.generatePure st n

instance DRG' ChaChaDRGInsecure where
  initialize = C.initializePure
  seedLength = 40
