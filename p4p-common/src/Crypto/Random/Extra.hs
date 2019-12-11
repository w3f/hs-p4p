{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Crypto.Random.Extra
  ( DRG'(..)
  , seedFromWords
  , naiveRandomWord16
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
import           Data.Word                  (Word16, Word64)
import           Foreign.Storable           (pokeElemOff)

import qualified Crypto.Cipher.ChaCha.Extra as C


-- | Like 'DRG' but supports initialisation from some arbitrary seed.
class DRG gen => DRG' gen where
  initialize :: ByteArrayAccess seed => seed -> gen

-- | Create a new Seed from 5-tuple of words64.
-- This interface is useful when creating a RNG out of tests generators (e.g. QuickCheck).
seedFromWords
  :: ByteArray seed => (Word64, Word64, Word64, Word64, Word64) -> seed
seedFromWords (a, b, c, d, e) = B.allocAndFreeze 40 fill
 where
  fill s =
    mapM_ (uncurry (pokeElemOff s)) [(0, a), (1, b), (2, c), (3, d), (4, e)]

naiveRandomWord16 :: DRG gen => Word16 -> gen -> (Word16, gen)
naiveRandomWord16 maxOut gen =
  undefined $ randomBytesGenerate @_ @ScrubbedBytes 8 gen


-- | ChaCha Deterministic Random Generator
type ChaChaDRG' = C.StatePure

instance DRG ChaChaDRG' where
  randomBytesGenerate n st =
    if n <= 0 then (B.empty, st) else C.generatePure st n

instance DRG' ChaChaDRG' where
  initialize = C.initializePure

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
