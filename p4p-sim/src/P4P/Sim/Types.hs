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
import           Data.Schedule         (Tick)
import           Data.Set              (Set)
import           Data.Void             (Void, absurd)
import           Data.Word             (Word16)
import           GHC.Generics          (Generic)
import           P4P.Proc              (GMsg (..), GMsgI, GMsgO, PAddr,
                                        Protocol (..), RuntimeI, RuntimeO)

-- internal
import           P4P.Sim.Numeric


-- | A pair that is slightly easier to type
data KV k v = !k :~ !v
  deriving (Eq, Ord, Show, Read, Generic)

-- | Process id type. This is internal to the simulation for convenience, and
-- is not made available to the process themselves.
type Pid = Word16

-- | Latency profile.
data SimLatency = SLatAddrIndep !KnownDistPos
    -- ^ Latency is independent of the addresses.
  deriving (Eq, Show, Read, Generic)

-- | State of the simulation. Process state is stored separately.
data SimState' i a = SimState
  { simNow     :: !Tick
  , simDRG     :: !ChaChaDRGInsecure
  , simLatency :: !SimLatency
  , simAddr    :: !(Map a (Set Pid))
  , simIn      :: !(Map Pid (Map Tick (SQ.Seq i)))
  }
  deriving (Eq, Show, Read, Generic)
makeLenses_ ''SimState'
type SimState ps = SimState' (GMsgI ps) (PAddr ps)

newSimState
  :: (ByteArrayAccess seed, Ord a)
  => seed
  -> SimLatency
  -> Set Pid
  -> SimState' i a
newSimState seed latency pids =
  SimState 0 (initialize seed) latency mempty (M.fromSet (const mempty) pids)

-- | Full state of the simulation, including the state of each process.
data SimFullState ps xs = SimFullState
  { simProcs  :: !(Map Pid ps)
  , simState  :: !(SimState ps)
  , simXState :: !xs
  }
  deriving (Generic, Functor)
-- note: we are forced to do this (instead of having 'SimFullState' ps i a xs'
-- like how 'SimState'' is defined) so that we can define @instance Protocol@
-- in a convenient way - because we can't apply type families in the instance
-- declaration.
deriving instance (Eq (Map Pid ps), Eq (SimState ps), Eq xs)
  => Eq (SimFullState ps xs)
deriving instance (Ord (Map Pid ps), Ord (SimState ps), Ord xs)
  => Ord (SimFullState ps xs)
deriving instance (Show (Map Pid ps), Show (SimState ps), Show xs)
  => Show (SimFullState ps xs)
deriving instance (Read (Map Pid ps), Read (SimState ps), Read xs)
  => Read (SimFullState ps xs)

data SimProcEvt' i o a =
  -- | A process received a message.
    SimMsgRecv !Pid !i
  -- | A process sent a message.
  | SimMsgSend !Pid !o
  -- | The user or a process tried to send to a non-existing pid.
  | SimNoSuchPid !(Either () Pid) !Pid
  -- | A process tried to send to an address with no listeners.
  | SimNoSuchAddr !Pid !a
 deriving (Eq, Ord, Show, Read, Generic)
makePrisms ''SimProcEvt'
type SimProcEvt ps = SimProcEvt' (GMsgI ps) (GMsgO ps) (PAddr ps)

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
data SimUserI' ps ui i a xi =
    SimProcUserI !Pid !ui
  | SimGetAllPids
  | SimGetAllInboxSizes
  | SimGetTickNow
  | SimGetState
  | SimProcAdd !Pid !(SimProcState ps i a)
  | SimProcGet !Pid
  | SimProcDel !Pid
  | SimExtensionI !xi
    -- ^ extension input type
 deriving (Eq, Ord, Show, Read, Generic, Functor)
type SimXUserI ps xs
  = SimUserI' ps (UserI ps) (GMsgI ps) (PAddr ps) (XUserI xs)
type SimUserI ps = SimXUserI ps ()

-- | User output from the sim. TODO: will be extended with debugging output.
data SimUserO' ps uo i a xo =
    SimProcUserO !Pid !uo
  | SimAllPids !(Set Pid)
  | SimAllInboxSizes !(Map Pid Int)
  | SimTickNow !Tick
  | SimGetStateResult !(Map Pid ps) !(SimState' i a) -- isomorphic to 'SimFullState ps ()'
  | SimProcAddResult !Pid !Bool
  | SimProcGetResult !Pid !(Maybe (SimProcState ps i a))
  | SimProcDelResult !Pid !(Maybe (SimProcState ps i a))
  | SimExtensionO !xo
    -- ^ extension output type
 deriving (Eq, Show, Read, Generic, Functor)
type SimXUserO ps xs
  = SimUserO' ps (UserO ps) (GMsgI ps) (PAddr ps) (XUserO xs)
type SimUserO ps = SimXUserO ps ()

data SimAuxO' ao i o a =
    SimUserAuxO !Pid !ao
  | SimProcEvent !(SimProcEvt' i o a)
  -- :^ TODO: filter out User messages, these are already represented elsewhere
 deriving (Eq, Ord, Show, Read, Generic)
type SimAuxO ps = SimAuxO' (AuxO ps) (GMsgI ps) (GMsgO ps) (PAddr ps)

data SimError = SimFailedReplayCompare
  { simFailedReplayCompareType     :: !String
  , simFailedReplayCompareTick     :: !Tick
  , simFailedReplayCompareExpected :: !String
  , simFailedReplayCompareActual   :: !String
  }
    -- ^ Failed to compare replay at the given tick.
  deriving (Eq, Ord, Show, Read, Generic)

class (
  PMsg xs ~ Void,
  AuxO xs ~ Void,
  Protocol xs
 ) => SimXProtocol ps xs where
  type XUserI xs
  type XUserO xs

  toUserI :: Either (SimUserO ps) (XUserI xs) -> Maybe (UserI xs)
  default toUserI
    :: (UserI xs ~ Either (SimUserO ps) (XUserI xs))
    => Either (SimUserO ps) (XUserI xs) -> Maybe (UserI xs)
  toUserI = Just

  fromUserO :: UserO xs -> Either (SimUserI ps) (XUserO xs)
  default fromUserO
    :: (UserO xs ~ Either (SimUserI ps) (XUserO xs))
    => UserO xs -> Either (SimUserI ps) (XUserO xs)
  fromUserO = id

instance SimXProtocol ps () where
  type XUserI () = Void
  type XUserO () = Void
  toUserI   = const Nothing
  fromUserO = absurd

-- these splices need to go after the class definition of SimXProtocol since it
-- is cyclicly dependent with these data structures
makePrisms ''SimUserI'
makePrisms ''SimUserO'
makePrisms ''SimAuxO'

instance SimXProtocol ps xs => Protocol (SimFullState ps xs) where
  type PMsg (SimFullState ps xs) = Void
  type UserI (SimFullState ps xs) = SimXUserI ps xs
  type UserO (SimFullState ps xs) = SimXUserO ps xs
  type AuxO (SimFullState ps xs) = SimAuxO ps

-- | Input into the sim.
type SimI ps = GMsg (RuntimeI ()) (SimUserI ps) Void Void
type SimXI ps xs = GMsg (RuntimeI ()) (SimXUserI ps xs) Void Void

-- | Output from the sim.
type SimO ps = GMsg (RuntimeO Void) (SimUserO ps) Void (SimAuxO ps)
type SimXO ps xs = GMsg (RuntimeO Void) (SimXUserO ps xs) Void (SimAuxO ps)
