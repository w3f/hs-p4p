module P4P.Proc.UnitTests where

-- external, test
import           Test.Tasty       hiding (after)
import           Test.Tasty.HUnit

-- external
import qualified Codec.Serialise  as SR
import qualified Data.Binary      as BN

import           Data.ByteString  (ByteString)
import           Data.Either      (fromRight)

-- internal
import           P4P.Proc
import           P4P.Proc.Types   (haskCodec16)


enc' :: Codec a -> a -> ByteString
enc' = (fromRight (error "unreachable") .) . codecEnc

dec' :: Codec a -> ByteString -> a
dec' = (fromRight (error "unreachable") .) . codecDec

roundTripExtVal :: IO ()
roundTripExtVal = do
  let xt = 123456789 :: Integer
  -- TODO: turn into a property check

  assertEqual "ExtVal cbor equal"
              (SR.serialise $ enc' cborCodec16 xt)
              (SR.serialise $ Val xt)
  assertEqual "ExtVal cbor equal"
              (enc' cborCodec16 xt)
              (SR.deserialise $ SR.serialise $ Val xt)
  assertEqual "ExtVal cbor equal"
              xt
              (dec' cborCodec16 $ SR.deserialise $ SR.serialise $ Val xt)

  assertEqual "ExtVal hask equal"
              (BN.encode $ enc' haskCodec16 xt)
              (BN.encode $ Val xt)
  assertEqual "ExtVal hask equal"
              (enc' haskCodec16 xt)
              (BN.decode $ BN.encode $ Val xt)
  assertEqual "ExtVal hask equal"
              xt
              (dec' haskCodec16 $ BN.decode $ BN.encode $ Val xt)

tests :: TestTree
tests = testGroup
  "P4P.Proc.UnitTests"
  [testCase "ExtVal serialisation round trips" roundTripExtVal]
