{-# LANGUAGE DataKinds #-}

{- | Utilities for writing simulations, for external use. -}
module P4P.Sim.Util
  ( PMut'
  , getEntropy
  , ChaChaDRGInsecure
  )
where

-- external
import           Crypto.Random.Entropy (getEntropy)
import           Crypto.Random.Extra   (ChaChaDRGInsecure)
import           Data.Primitive.MutVar (MutVar (..))
import           P4P.Proc.Instances    (PMut (..), PrimMonad (..),
                                        PrimOpGroup (..))


type PMut' = PMut (MutVar (PrimState IO)) 'OpST (PrimState IO)
