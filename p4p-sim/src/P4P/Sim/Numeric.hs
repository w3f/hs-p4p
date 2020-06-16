{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module P4P.Sim.Numeric where

import qualified Statistics.Distribution.Pure      as D

import           Crypto.Random                     (DRG)
import           GHC.Generics                      (Generic)
import           Statistics.Distribution.Lognormal (LognormalDistribution,
                                                    lognormalDistrMeanStddevErr)
import           Statistics.Distribution.Weibull   (WeibullDistribution, weibullDistrApproxMeanStddevErr)


-- | A mean & standard deviation.
data MeanStddev = MeanStddev !Double !Double
  deriving (Eq, Ord, Show, Read, Generic)

-- | A known probability distribution, positive (i.e. > 0).
data KnownDistPos' dc dln dw =
    DistConstant !dc
  | DistLognormal !dln
  | DistWeibull !dw
 deriving (Eq, Ord, Show, Read, Generic)

-- | Alias for the external representation.
--
-- Log-normal is specified by its mean and std-dev.
-- Weibull is specified by its mean and std-dev.
type KnownDistPosExt = KnownDistPos' Double MeanStddev MeanStddev

-- | Alias for the internal representation.
type KnownDistPos
  = KnownDistPos' Double LognormalDistribution WeibullDistribution

distPosToInternal :: KnownDistPosExt -> Either String KnownDistPos
distPosToInternal = \case
  DistConstant a -> Right $ DistConstant (realToFrac a)
  DistLognormal (MeanStddev m s) ->
    DistLognormal <$> lognormalDistrMeanStddevErr m s
  DistWeibull (MeanStddev m s) ->
    DistWeibull <$> weibullDistrApproxMeanStddevErr m s

sampleDist :: DRG g => KnownDistPos -> g -> (Double, g)
sampleDist = \case
  DistConstant  k -> (k, )
  DistLognormal d -> D.drawCont d
  DistWeibull   d -> D.drawCont d
