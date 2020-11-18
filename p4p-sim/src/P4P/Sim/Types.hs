{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module P4P.Sim.Types where

-- external
import qualified Data.Map.Strict       as M
import qualified Data.Sequence         as SQ
import qualified Data.Set              as S

import           Codec.Serialise       (Serialise)
import           Control.Lens.TH       (makePrisms)
import           Control.Lens.TH.Extra (makeLenses_)
import           Crypto.Random.Extra   (ByteArrayAccess, ChaChaDRGInsecure,
                                        initialize)
import           Data.Binary           (Binary)
import           Data.Map.Strict       (Map)
import           Data.Schedule         (HasNow (..), Tick)
import           Data.Set              (Set)
import           Data.Void             (Void, absurd)
import           Data.Word             (Word16)
import           GHC.Generics          (Generic)
import           P4P.Proc              (GMsgI, GMsgO, PMsgI, PMsgI, PMsgO,
                                        ProcIface (..), UProtocol (..))

-- internal
import           P4P.Sim.Numeric


-- | Process id type. This is internal to the simulation for convenience, and
-- is not made available to the process themselves.
type Pid = Word16

-- | Latency profile.
data SimLatency = SLatAddrIndep !KnownDistPos
    -- ^ Latency is independent of the addresses.
  deriving (Eq, Show, Read, Generic, Binary, Serialise)

-- | State of the simulation. Process state is stored separately.
data SimState' i a = SimState
  { simNow     :: !Tick
  , simDRG     :: !ChaChaDRGInsecure
  , simLatency :: !SimLatency
  , simAddr    :: !(Map a (Set Pid))
  , simIn      :: !(Map Pid (Map Tick (SQ.Seq i)))
  }
  deriving (Eq, Show, Read, Generic, Binary, Serialise)
makeLenses_ ''SimState'
type SimState ps = SimState' (PMsgI ps) (Addr ps)

newSimState
  :: (ByteArrayAccess seed, Ord a)
  => seed
  -> SimLatency
  -> Map Pid (Set a)
  -> SimState' i a
newSimState seed latency addrByPid = SimState 0
                                              (initialize seed)
                                              latency
                                              addrMap
                                              mempty
 where
  addrMap = M.foldrWithKey' populate M.empty addrByPid
  populate pid addrs m = S.foldr' (M.alter f) m addrs
   where
    f = \case
      Nothing   -> Just (S.singleton pid)
      Just pids -> Just (S.insert pid pids)

-- | Full state of the simulation, including the state of each process.
data SimFullState ps xs = SimFullState
  { simProcs  :: !(Map Pid ps)
  , simState  :: !(SimState ps)
  , simXState :: !xs
  }
  deriving (Generic, Functor)
-- note: we are forced to do this (instead of having 'SimFullState' ps i a xs'
-- like how 'SimState'' is defined) so that we can define @instance ProcIface@
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
instance (Binary (Map Pid ps), Binary (SimState ps), Binary xs) => Binary (SimFullState ps xs)
instance (Serialise (Map Pid ps), Serialise (SimState ps), Serialise xs) => Serialise (SimFullState ps xs)

data SimProcEvt' i o a =
  -- | A process received a message.
    SimMsgRecv !Pid !i
  -- | A process sent a message.
  | SimMsgSend !Pid !o
  -- | The user or a process tried to send to a non-existing pid.
  | SimNoSuchPid !(Either () Pid) !Pid
  -- | A process tried to send to an address with no listeners.
  | SimNoSuchAddr !Pid !a
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)
makePrisms ''SimProcEvt'
type SimProcEvt ps = SimProcEvt' (PMsgI ps) (PMsgO ps) (Addr ps)

-- | All state relating to a process in the simulation.
--
-- This is only used for 'SimProcAdd' and 'SimProcDel', the actual state is
-- stored in a slightly different form.
data SimProcState ps i a = SimProcState
  { spAddr  :: !(Set a)
  , spInbox :: !(Map Tick (SQ.Seq i))
  , spState :: !ps
  }
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

-- | User input into the sim. TODO: will be extended with debugging commands.
data SimHiI' ps ui i a xi =
    SimProcHiI !Pid !ui
  | SimResetAddrs
  | SimGetAllPids
  | SimGetAllInboxSizes
  | SimGetTickNow
  | SimGetState !Int
  | SimProcAdd !Pid !(SimProcState ps i a)
  | SimProcGet !Pid
  | SimProcDel !Pid
  | SimExtensionI !xi
    -- ^ extension input type
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise, Functor)
type SimXHiI ps xs = SimHiI' ps (HiI ps) (PMsgI ps) (Addr ps) (XHiI xs)
type SimHiI ps = SimXHiI ps ()

-- | User output from the sim. TODO: will be extended with debugging output.
data SimHiO' ps uo i a xo =
    SimProcHiO !Pid !uo
  | SimAllPids !(Set Pid)
  | SimAllInboxSizes !(Map Pid Int)
  | SimTickNow !Tick
  | SimGetStateResult !(Map Pid ps) !(SimState' i a) -- isomorphic to 'SimFullState ps ()'
  | SimProcAddResult !Pid !Bool
  | SimProcGetResult !Pid !(Maybe (SimProcState ps i a))
  | SimProcDelResult !Pid !(Maybe (SimProcState ps i a))
  | SimExtensionO !xo
    -- ^ extension output type
 deriving (Eq, Show, Read, Generic, Binary, Serialise, Functor)
type SimXHiO ps xs = SimHiO' ps (HiO ps) (PMsgI ps) (Addr ps) (XHiO xs)
type SimHiO ps = SimXHiO ps ()

data SimAuxO' ao i o a =
    SimUserAuxO !Pid !ao
  | SimProcEvent !(SimProcEvt' i o a)
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)
type SimAuxO ps = SimAuxO' (AuxO ps) (PMsgI ps) (PMsgO ps) (Addr ps)

class (ProcIface xs, AuxO xs ~ Void) => SimXProcIface ps xs where
  type XHiI xs
  type XHiO xs

  toHiI :: Either (SimHiO ps) (XHiI xs) -> Maybe (HiI xs)
  default toHiI
    :: (HiI xs ~ Either (SimHiO ps) (XHiI xs))
    => Either (SimHiO ps) (XHiI xs) -> Maybe (HiI xs)
  toHiI = Just

  fromHiO :: HiO xs -> Either (SimHiI ps) (XHiO xs)
  default fromHiO
    :: (HiO xs ~ Either (SimHiI ps) (XHiO xs))
    => HiO xs -> Either (SimHiI ps) (XHiO xs)
  fromHiO = id

instance SimXProcIface ps () where
  type XHiI () = Void
  type XHiO () = Void
  toHiI   = const Nothing
  fromHiO = absurd

-- these splices need to go after the class definition of SimXProcIface since it
-- is cyclicly dependent with these data structures
makePrisms ''SimHiI'
makePrisms ''SimHiO'
makePrisms ''SimAuxO'

instance SimXProcIface ps xs => ProcIface (SimFullState ps xs) where
  type LoI (SimFullState ps xs) = Void
  type LoO (SimFullState ps xs) = Void
  type HiI (SimFullState ps xs) = SimXHiI ps xs
  type HiO (SimFullState ps xs) = SimXHiO ps xs
  type AuxO (SimFullState ps xs) = SimAuxO ps

instance HasNow (SimFullState ps xs) where
  getNow = simNow . simState

-- | Input into the sim.
type SimI ps = GMsgI Tick Void (SimHiI ps)
type SimXI ps xs = GMsgI Tick Void (SimXHiI ps xs)

-- | Output from the sim.
type SimO ps = GMsgO (SimAuxO ps) Void (SimHiO ps)
type SimXO ps xs = GMsgO (SimAuxO ps) Void (SimXHiO ps xs)
type SimXO' ps xs = GMsgO Void Void (SimXHiO ps xs)
