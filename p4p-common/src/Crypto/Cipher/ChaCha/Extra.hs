{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.Cipher.ChaCha.Extra
  ( StatePure(..)
  , StatePureInsecure(..)
  , initializePure
  , generatePure
  , module Crypto.Cipher.ChaCha
  )
where

import           Codec.Serialise      (Serialise)
import           Control.DeepSeq      (NFData)
import           Data.Binary          (Binary)
import           Data.ByteArray       (ByteArray, ByteArrayAccess)
import qualified Data.ByteArray       as B
import qualified Data.ByteString      as BS
import           Data.String          (IsString)
import           Data.Word            (Word8)
import           Foreign.C.Types
import           Foreign.Ptr
import           System.IO.Unsafe     (unsafeDupablePerformIO)

import           GHC.Generics         (Generic)

import           Crypto.Cipher.ChaCha

-- | ChaCha context for DRG purpose (see Crypto.Random.ChaChaDRG)
-- Same as 'StateSimple' except it derives more typeclasses.
newtype StatePure = StatePure B.ScrubbedBytes
    deriving (Eq, Ord, Show, Generic, IsString, Semigroup, Monoid, NFData, ByteArray, ByteArrayAccess)

-- | ChaCha context for DRG purpose (see Crypto.Random.ChaChaDRG)
-- Same as 'StateSimple' except it derives more typeclasses, including Read
-- for simulations. It does not zero the memory when freed, hence insecure.
newtype StatePureInsecure = StatePureInsecure BS.ByteString
    deriving (Eq, Ord, Show, Read, Generic, IsString, Semigroup, Monoid, NFData, ByteArray, ByteArrayAccess)
instance Binary StatePureInsecure
instance Serialise StatePureInsecure

-- | Initialize simple ChaCha State
--
-- The seed need to be at least 40 bytes long
initializePure
  :: (ByteArrayAccess seed, ByteArray st)
  => seed -- ^ a 40 bytes long seed
  -> st
initializePure seed
  | sLen < 40 = error "ChaCha Random: seed length should be 40 bytes"
  | otherwise = unsafeDupablePerformIO $ do
    B.alloc 64 $ \stPtr -> B.withByteArray seed $ \seedPtr ->
      ccryptonite_chacha_init_core stPtr 32 seedPtr 8 (seedPtr `plusPtr` 32)
  where sLen = B.length seed

-- | Similar to 'generate' but assume certains values
generatePure :: (ByteArray ba, ByteArray st) => st -> Int -> (ba, st)
generatePure prevSt nbBytes = unsafeDupablePerformIO $ do
  newSt  <- B.copy prevSt (\_ -> return ())
  output <- B.alloc nbBytes $ \dstPtr -> B.withByteArray newSt $ \stPtr ->
    ccryptonite_chacha_random 8 dstPtr stPtr (fromIntegral nbBytes)
  return (output, newSt)

foreign import ccall "cryptonite_chacha_init_core"
    ccryptonite_chacha_init_core :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> IO ()

foreign import ccall "cryptonite_chacha_random"
    ccryptonite_chacha_random :: Int -> Ptr Word8 -> Ptr Word8 -> CUInt -> IO ()
