{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell   #-}

module P4P.Sim.Types where

-- external
import qualified Data.Map.Strict       as M
import qualified Data.Sequence         as SQ

import           Control.Lens.TH       (makePrisms)
import           Control.Lens.TH.Extra (makeLenses_)
import           Crypto.Random.Extra   (ByteArrayAccess, ChaChaDRGInsecure,
                                        initialize)
import           Data.Map.Strict       (Map)
import           Data.Schedule         (Tick, TickDelta)
import           Data.Set              (Set)
import           Data.Void             (Void)
import           GHC.Generics          (Generic)
import           P4P.Proc              (GMsg (..), GMsgI, GMsgO, PAddr,
                                        Protocol (..))


-- | A pair that is slightly easier to type
data KV k v = !k :~ !v deriving (Eq, Ord, Show, Read, Generic)

data SimProcEvt' pid i o a =
  -- | A process received a message.
    SimMsgRecv !pid !i
  -- | A process sent a message.
  | SimMsgSend !pid !o
  -- | The user or a process tried to send to a non-existing pid.
  | SimNoSuchPid !(Either () pid) !pid
  -- | A process tried to send to an address with no listeners.
  | SimNoSuchAddr !pid !a
 deriving (Eq, Ord, Show, Read, Generic)
makePrisms ''SimProcEvt'

type SimProcEvt pid ps = SimProcEvt' pid (GMsgI ps) (GMsgO ps) (PAddr ps)

-- | Runtime input to the sim. TODO: will be extended with debugging commands.
data SimRuntimeI =
    SimRTTick !Tick
 deriving (Eq, Ord, Show, Read, Generic)

-- | Runtime output to the sim. TODO: will be extended with debugging output.
data SimRuntimeO
 deriving (Eq, Ord, Show, Read, Generic)

type SimI pid ps = GMsg SimRuntimeI (KV pid (UserI ps)) Void
type SimO pid ps = GMsg SimRuntimeO (KV pid (UserO ps)) (SimProcEvt pid ps)


-- | A known probability distribution, non-negative.
data KnownDistNonNeg a =
    DistConstant !a
  | DistLogNormal !a !a
    -- ^ Log-normal-distributed with a given mean and std-dev
  | DistWeibull !a !a
    -- ^ Weibull-distributed with a given mean and std-dev
 deriving (Eq, Ord, Show, Read, Generic)

-- | Latency profile.
data SimLatency =
    -- | Latency is independent of the addresses.
    SLatAddrIndep !(KnownDistNonNeg TickDelta)
 deriving (Eq, Ord, Show, Read, Generic)

data SimState' pid i a = SimState {
    simNow     :: !Tick
  , simDRG     :: !ChaChaDRGInsecure
  , simLatency :: !SimLatency
  , simAddr    :: !(Map a (Set pid))
  , simIn      :: !(Map pid (Map Tick (SQ.Seq i)))
  }
 deriving (Eq, Ord, Show, Read, Generic)
makeLenses_ ''SimState'

type SimState pid ps = SimState' pid (GMsgI ps) (PAddr ps)

newSimState
  :: (ByteArrayAccess seed, Ord a)
  => seed
  -> SimLatency
  -> Set pid
  -> SimState' pid i a
newSimState seed latency pids =
  SimState 0 (initialize seed) latency mempty (M.fromSet (const mempty) pids)

data SimError =
    SimFailedReplayCompare
    { simFailedReplayCompareType     :: !String
    , simFailedReplayCompareTick     :: !Tick
    , simFailedReplayCompareExpected :: !String
    , simFailedReplayCompareActual   :: !String
    }
    -- ^ Failed to compare replay at the given tick.
 deriving (Eq, Ord, Show, Read, Generic)
