{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- GHC bug, default implementations for a class includes subclass constraint
-- https://gitlab.haskell.org/ghc/ghc/issues/18259
{-# OPTIONS_GHC -Wno-error=redundant-constraints #-}

module P4P.Proc.Internal where

-- external
import           Control.Monad (join, void)
import           Control.Op
import           Data.Kind     (Constraint, Type)
import           Data.Schedule (Tick, whileJustM)
import           Data.Void     (Void)
import           GHC.Generics  (Generic)


{- | Protocol message. -}
class ProtoMsg msg where
  -- | Type of protocol address. Typically, messages embed the address type
  -- since they need to specify recipients and other entities, so this is why
  -- this is not polymorphic.
  type Addr msg

  -- | Get the target of a message sent by a process, used by the execution
  -- environment to decide how to send an outgoing message.
  getTarget :: msg -> Addr msg

  -- | Set the source of a message received by a process, used by the execution
  -- environment to hint the receipt of an incoming message.
  setSource :: Addr msg -> msg -> msg

-- | Void message, for local-only processes such as simulations.
instance ProtoMsg Void where
  type Addr Void = Void
  getTarget = id
  setSource = const id

-- | Input from the runtime execution environment to a process.
--
-- The 'task' type-param is provided in case users want to re-use this data
-- structure for other purposes; the simulation framework instantiates it to
-- '()'.
data RuntimeI task = RTTick !Tick !task
  deriving (Eq, Ord, Show, Read, Generic)

-- | Output from a process to the runtime execution environment.
data RuntimeO addr = RTAddr ![addr] ![addr]
  deriving (Eq, Ord, Show, Read, Generic)

-- | Protocol with its protocol-messages and user-messages.
class ProtoMsg (PMsg ps) => Protocol ps where
  -- | Protocol message type.
  type PMsg ps :: Type
  -- | Type of message from the user to a process.
  type UserI ps :: Type
  -- | Type of message from a process to the user.
  type UserO ps :: Type
  -- | Type of auxiliary message from a process.
  type AuxO ps :: Type
  type AuxO ps = Void

-- | Empty (unit) protocol, useful for composing with other protocols.
instance Protocol () where
  type PMsg () = Void
  type UserI () = Void
  type UserO () = Void
  type AuxO () = Void

type PAddr ps = Addr (PMsg ps)

{- | General message, from/to the runtime, the user, or another process. -}
data GMsg r u p a
  = MsgRT !r
  -- ^ Message from/to the runtime.
  | MsgUser !u
  -- ^ Message from/to the user, or another trusted local process.
  | MsgProc !p
  -- ^ Message from/to an external untrusted process.
  | MsgAux !a
  -- ^ Message from/to auxillary sources, e.g. for logging or debugging
  -- purposes. This is exempted from the deterministic behaviour contract.
 deriving (Eq, Ord, Show, Read, Generic)

type GMsgI ps = GMsg (RuntimeI ()) (UserI ps) (PMsg ps) Void
type GMsgO ps = GMsg (RuntimeO (Addr (PMsg ps))) (UserO ps) (PMsg ps) (AuxO ps)

{- | Pure communicating process.

If your state type implements this, then deriving an instance of 'Process'
becomes trivial - all default methods will work. See also 'Instances.hs' for
some automatic 'Process' instances of Lens/MutVars wrapped around the state.
-}
class Protocol ps => Proc ps where

  -- | Receive-address(es) of a process.
  -- TODO: maybe this should be deleted and be represented in the EnvI instead,
  -- similar to time events.
  getAddrs :: ps -> [Addr (PMsg ps)]

  -- | Current tick of a process.
  localNow :: ps -> Tick

  -- | React to inputs.
  react :: GMsgI ps -> ps -> ([GMsgO ps], ps)

instance Proc () where
  getAddrs () = []
  localNow () = 0
  react v () = ([], ())

{- | Communicating process.

Processes must be deterministic and interact with their environment only
via 'react'. In other words:

  * 'pure state' is equal to 'proceed p state >> suspend p'.
  * 'suspend p >>= proceed p' must have no meaningful side-effects.
  * Any meaningful side-effects of 'react' must be fully-represented within
    'p' as part of 'State p'.

Equivalently, 'execute' on any inputs must be deterministic in the outputs and
all side-effects of its computation context 'm' must be not meaningful.

The phrase "meaningful side-effects" can be interpreted loosely (e.g. temp
files and logging output may be ignored) but should take into account as much
as possible, at least all things relevant to a protocol - including any
timeout-based behaviours, RNG internal secret states, etc.

A process that is completely pure in its implementation need not provide an
instance of this class directly, but rather should provide an instance of
'Proc' and then use one of our instances for 'PureProcess', listed below. OTOH
a process that must "cheat" can implement this class directly, as long as it
has deterministic behaviour. For example, if it wants to perform disk accesses
directly for convenience during initial development, where the risk of failure
is negligible. Note however that eventually this should be fixed.

A @'Process' p@ is similar but different from an 'Control.Arrow.Arrow':

- its input and output types are fixed via the associated type families of
  @'Protocol' ('State' p)@ (i.e. more specific than 'Control.Arrow.Arrow').

- it produces any number of outputs from a single input (i.e. more general than
  'Arrow'). Standard ways of composing 'Process'es are also different from the
  standard ways of composing 'Control.Arrow.Arrow' - see 'P4P.Proc.Util'.
-}
class Protocol (State p) => Process p where
  -- | Type of process state
  type State p
  -- | Constraint over execution environment type, e.g. 'Monad'.
  type Ctx p (m :: Type -> Type) :: Constraint

  -- | Create a new process with the given state.
  proceed :: Ctx p m => State p -> m p

  -- | Destroy a process and return its state.
  --
  -- The old process must never be used again.
  suspend :: Ctx p m => p -> m (State p)

  -- | Receive-address(es) of a process.
  --
  -- A process has the power to say what its own receive-addresses are, but how
  -- this behaves is entirely up to the underlying execution environment. For
  -- example, it may ignore some addresses, or it make only make changes after
  -- certain time intervals, or whatever. Messages may even be delivered even
  -- if this list is empty; if the process really wants to enforce "ignore all
  -- input" then this should be done in 'react'.
  getAddrsM :: Ctx p m => p -> m [ProcAddr p]

  -- | Current tick of a process.
  localNowM :: Ctx p m => p -> m Tick

  -- | React to inputs.
  reactM :: Ctx p m => p -> ProcMsgI p -> m [ProcMsgO p]

  default getAddrsM
    :: (Ctx p m, PureProcess p, Proc (State p))
    => p -> m [ProcAddr p]
  getAddrsM pt = runPure pt $ \s -> (getAddrs s, s)

  default localNowM
    :: (Ctx p m, PureProcess p, Proc (State p))
    => p -> m Tick
  localNowM pt = runPure pt $ \s -> (localNow s, s)

  default reactM
    :: (Ctx p m, PureProcess p, Proc (State p))
    => p -> ProcMsgI p -> m [ProcMsgO p]
  reactM pt = runPure pt . react

-- | A process that can embed arbitrary pure computations over its state.
class Process p => PureProcess p where
  -- | Run a pure state-machine in the process.
  runPure :: Ctx p m => p -> (State p -> (a, State p)) -> m a

-- | Type alias for the address of a process.
type ProcAddr p = PAddr (State p)

-- | Type alias for the messages of a process.
type ProcMsgI p = GMsgI (State p)
type ProcMsgO p = GMsgO (State p)

-- | React to several inputs.
reactAllM
  :: (Process p, Ctx p m, Monad m) => p -> [ProcMsgI p] -> m [ProcMsgO p]
reactAllM p pinputs = pinputs >$> reactM p |> sequence >$> join

-- | Run some operation on a process, as a state transition on its state.
asState
  :: (Process p, Ctx p m, Monad m) => (p -> m a) -> State p -> m (a, State p)
asState runProcess pstate = do
  p <- proceed pstate
  r <- runProcess p
  s <- suspend p
  pure (r, s)

-- | Simple model of a process environment, for 'reactEnv'.
data ProcEnv ps m = ProcEnv
  { envGuard :: !(forall b. ((forall a. m a -> m a) -> m b) -> m b)
  -- ^ Custom guard function, e.g. 'Control.Exception.mask' or something else.
  , envI     :: !(m (Maybe (GMsgI ps)))
  -- ^ Get an input, or EOF.
  , envO     :: !(Tick -> [GMsgO ps] -> m ())
  -- ^ Deal with a batch of outputs.
  }

-- | React to inputs, and handle outputs, with the given actions.
reactEnv :: (Process p, Monad m, Ctx p m) => ProcEnv (State p) m -> p -> m ()
reactEnv ProcEnv {..} proc = void $ envGuard $ \unguard ->
  whileJustM $ unguard $ do
    realNow <- localNowM proc
    -- we tick over after handling RTTick, so use realNow from before handling it
    (envI >>=) $ traverse $ \i -> do
      outs <- reactM proc i
      envO realNow outs
      pure ()
