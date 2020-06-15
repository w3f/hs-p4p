{-# LANGUAGE MultiWayIf #-}

module Statistics.Distribution.Pure where

import           Crypto.Random.Extra     (DRG (..), generateUntil,
                                          randomProbGenerate)
import           Statistics.Distribution

-- | Draw from a continuous distribution using the inverse cumulative density
-- function.
drawCont :: (ContDistr d, DRG g) => d -> g -> (Double, g)
drawCont d g0 =
  let (p, g1) = generateUntil (/= 0.0) randomProbGenerate g0
  in  (quantile d p, g1)

-- | Draw from a discrete distribution using a sequence of Bernoulli draws.
fromPMF :: DRG g => (Int -> Double) -> g -> (Int, g)
fromPMF prob g0 = f 0 1 g0
 where
  f i r g =
    let q       = prob i
        (p, g1) = randomProbGenerate g0
    in  if
          | r < 0          -> error "fromPMF: total PMF above 1"
          | q < 0 || q > 1 -> error "fromPMF: invalid probability value"
          | p * r < q      -> (i, g1)
          | i <= 0         -> f (1 - i) (r - q) g1
          | otherwise      -> f (-i) (r - q) g1

-- | Draw from a non-negative discrete distribution using the probability mass
-- function.
drawDisc :: (DiscreteDistr d, DRG g) => d -> g -> (Int, g)
drawDisc = fromPMF . probability
