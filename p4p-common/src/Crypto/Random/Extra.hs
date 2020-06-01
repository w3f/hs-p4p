{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Crypto.Random.Extra
  ( DRG'(..)
  , initializeFrom
  , ChaChaDRG'
  , ChaChaDRGInsecure
  , module Data.ByteArray
  , module Crypto.Random
  )
where

import           Crypto.Random
import           Data.ByteArray             (ByteArray, ByteArrayAccess,
                                             ScrubbedBytes)
import qualified Data.ByteArray             as B

import qualified Crypto.Cipher.ChaCha.Extra as C


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
