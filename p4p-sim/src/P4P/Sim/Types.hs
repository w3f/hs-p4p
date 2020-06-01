{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           Data.Void             (Void, absurd)
import           GHC.Generics          (Generic)
import           P4P.Proc              (GMsg (..), GMsgI, GMsgO, PAddr,
                                        Protocol (..), RuntimeI, RuntimeO)


-- | A pair that is slightly easier to type
data KV k v = !k :~ !v
  deriving (Eq, Ord, Show, Read, Generic)

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

-- | All state relating to a process in the simulation.
--
-- This is only used for 'SimProcAdd' and 'SimProcDel', the actual state is
-- stored in a slightly different form.
data SimProcState ps i a = SimProcState
  { spAddr  :: !(Set a)
  , spInbox :: !(Map Tick (SQ.Seq i))
  , spState :: !ps
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | User input into the sim. TODO: will be extended with debugging commands.
data SimUserI' pid ps ui i a xi =
    SimProcUserI !pid !ui
  | SimGetAllPids
  | SimProcAdd !pid !(SimProcState ps i a)
  | SimProcDel !pid
  | SimExtensionI !xi
    -- ^ extension input type
 deriving (Eq, Ord, Show, Read, Generic, Functor)
type SimXUserI pid ps xs
  = SimUserI' pid ps (UserI ps) (GMsgI ps) (PAddr ps) (XUserI xs)
type SimUserI pid ps = SimXUserI pid ps ()

-- | User output from the sim. TODO: will be extended with debugging output.
data SimUserO' pid ps uo i a xo =
    SimProcUserO !pid !uo
  | SimAllPids !(Set pid)
  | SimProcAddResult !pid !Bool
  | SimProcDelResult !pid !(Maybe (SimProcState ps i a))
  | SimExtensionO !xo
    -- ^ extension output type
 deriving (Eq, Ord, Show, Read, Generic, Functor)
type SimXUserO pid ps xs
  = SimUserO' pid ps (UserO ps) (GMsgI ps) (PAddr ps) (XUserO xs)
type SimUserO pid ps = SimXUserO pid ps ()

data SimAuxO' pid ao i o a =
    SimUserAuxO !pid !ao
  | SimProcEvent !(SimProcEvt' pid i o a)
  -- :^ TODO: filter out User messages, these are already represented elsewhere
 deriving (Eq, Ord, Show, Read, Generic)
type SimAuxO pid ps = SimAuxO' pid (AuxO ps) (GMsgI ps) (GMsgO ps) (PAddr ps)

class (
  PMsg xs ~ Void,
  AuxO xs ~ Void,
  Protocol xs
 ) => SimXProtocol pid ps xs where
  type XUserI xs
  type XUserO xs

  toUserI :: Either (SimUserO pid ps) (XUserI xs) -> Maybe (UserI xs)
  default toUserI
    :: (UserI xs ~ Either (SimUserO pid ps) (XUserI xs))
    => Either (SimUserO pid ps) (XUserI xs) -> Maybe (UserI xs)
  toUserI = Just

  fromUserO :: UserO xs -> Either (SimUserI pid ps) (XUserO xs)
  default fromUserO
    :: (UserO xs ~ Either (SimUserI pid ps) (XUserO xs))
    => UserO xs -> Either (SimUserI pid ps) (XUserO xs)
  fromUserO = id

instance SimXProtocol pid ps () where
  type XUserI () = Void
  type XUserO () = Void
  toUserI   = const Nothing
  fromUserO = absurd

-- these splices need to go after the class definition of SimXProtocol since it
-- is cyclicly dependent with these data structures
makePrisms ''SimUserI'
makePrisms ''SimUserO'
makePrisms ''SimAuxO'

-- | A known probability distribution, non-negative.
data KnownDistNonNeg a =
    DistConstant !a
  | DistLogNormal !a !a
    -- ^ Log-normal-distributed with a given mean and std-dev
  | DistWeibull !a !a
    -- ^ Weibull-distributed with a given mean and std-dev
 deriving (Eq, Ord, Show, Read, Generic)

-- | Latency profile.
data SimLatency = SLatAddrIndep !(KnownDistNonNeg TickDelta)
    -- ^ Latency is independent of the addresses.
  deriving (Eq, Ord, Show, Read, Generic)

-- | State of the simulation.
--
-- Note that process state is stored separately.
data SimState' pid i a = SimState
  { simNow     :: !Tick
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

data SimError = SimFailedReplayCompare
  { simFailedReplayCompareType     :: !String
  , simFailedReplayCompareTick     :: !Tick
  , simFailedReplayCompareExpected :: !String
  , simFailedReplayCompareActual   :: !String
  }
    -- ^ Failed to compare replay at the given tick.
  deriving (Eq, Ord, Show, Read, Generic)

-- | Full state of the simulation.
data SimFullState pid ps xs = SimFullState
  { simProcs  :: !(Map pid ps)
  , simState  :: !(SimState pid ps)
  , simXState :: !xs
  }
  deriving (Generic, Functor)
-- note: we are forced to do this to define @instance Protocol@, because we
-- can't apply type families in the instance declaration.
deriving instance (Eq (Map pid ps), Eq (SimState pid ps), Eq xs)
  => Eq (SimFullState pid ps xs)
deriving instance (Ord (Map pid ps), Ord (SimState pid ps), Ord xs)
  => Ord (SimFullState pid ps xs)
deriving instance (Show (Map pid ps), Show (SimState pid ps), Show xs)
  => Show (SimFullState pid ps xs)
deriving instance (Read (Map pid ps), Read (SimState pid ps), Read xs)
  => Read (SimFullState pid ps xs)

instance SimXProtocol pid ps xs => Protocol (SimFullState pid ps xs) where
  type PMsg (SimFullState pid ps xs) = Void
  type UserI (SimFullState pid ps xs) = SimXUserI pid ps xs
  type UserO (SimFullState pid ps xs) = SimXUserO pid ps xs
  type AuxO (SimFullState pid ps xs) = SimAuxO pid ps

-- | Input into the sim.
type SimI pid ps = GMsg (RuntimeI ()) (SimUserI pid ps) Void Void
type SimXI pid ps xs = GMsg (RuntimeI ()) (SimXUserI pid ps xs) Void Void

-- | Output from the sim.
type SimO pid ps = GMsg (RuntimeO Void) (SimUserO pid ps) Void (SimAuxO pid ps)
type SimXO pid ps xs
  = GMsg (RuntimeO Void) (SimXUserO pid ps xs) Void (SimAuxO pid ps)
