{-# LANGUAGE OverloadedStrings #-}

module P4P.Protocol.DHT.Kademlia.UnitTests where

-- external
import           Test.Tasty                       hiding (after)
import           Test.Tasty.HUnit

import qualified Data.ByteString                  as BS

import           Control.Monad.Trans.Except       (runExceptT)
import           Control.Monad.Trans.State.Strict (runStateT, state)
import           Crypto.Random.Extra              (ChaChaDRG', seedFromWords)
import           Data.Maybe                       (fromJust)
import           Data.Traversable                 (for)

-- internal
import           P4P.Protocol.DHT.Kademlia
import           P4P.Protocol.DHT.Kademlia.State


smoke :: IO ()
smoke = do
  let self = "00000000000000000000000000000000"
  let seed = seedFromWords (0, 0, 0, 0, 0) :: BS.ByteString
  let s0   = emptyState self mempty seed defaultParams' :: State ChaChaDRG'
  r <- runExceptT $ checkState s0
  assertEqual "emptyState passed check" r (Right ())
  let res = kGetNodes self s0
  assertBool "lookup self doesn't work on empty state" $ null res

  -- test newNodeIdR, it's very fiddly
  let idxes = fromIntegral <$> [0 .. pred (parHBits defaultParams')]
  (nIds, s1) <- flip runStateT s0 $ for idxes $ \idx -> do
    state $ \s -> newNodeIdR (kBucketIndexToPrefix idx s) s
  let idxes' = fmap (fromJust . (`kBucketGetIndex` s1)) nIds
  assertEqual "random indexes passed check" idxes idxes'


tests :: TestTree
tests =
  testGroup "P4P.Protocol.DHT.Kademlia.UnitTests" [testCase "smoke" smoke]
