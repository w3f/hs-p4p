{-# LANGUAGE DataKinds #-}

{- | Utilities for writing simulations, for external use. -}
module P4P.Sim.Util
  ( Pid
  , PMut'
  , mkInitPids
  , getEntropy
  , ChaChaDRGInsecure
  )
where

-- external
import qualified Data.Set              as S

import           Crypto.Random.Entropy (getEntropy)
import           Crypto.Random.Extra   (ChaChaDRGInsecure)
import           Data.Primitive.MutVar (MutVar (..))
import           Data.Word             (Word64)
import           P4P.Proc.Instances    (PMut (..), PrimMonad (..),
                                        PrimOpGroup (..))

-- internal
import           P4P.Sim.Options

type Pid = Word64
type PMut' = PMut (MutVar (PrimState IO)) 'OpST (PrimState IO)

mkInitPids :: SimOptions -> S.Set Pid
mkInitPids opt = S.fromList [0 .. pred (fromIntegral (simInitNodes opt))]
