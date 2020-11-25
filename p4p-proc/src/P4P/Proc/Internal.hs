{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- GHC bug, default implementations for a class includes subclass constraint
-- https://gitlab.haskell.org/ghc/ghc/issues/18259
{-# OPTIONS_GHC -Wno-error=redundant-constraints #-}

module P4P.Proc.Internal where

-- external
import           Control.Monad                    (join)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.State.Strict (execStateT, state)
import           Control.Op
import           Data.Kind                        (Constraint, Type)
import           Data.Schedule                    (Tick)
import           Data.Void                        (Void)

-- internal
import           P4P.Proc.Types


{- | Message interface of a 'Process'.

Our processes communicate solely via message-passing; this interface defines
the types of the messages.  Specifically, messages may be sent to and received
from these external components:

- a "lower" layer, which by convention provides services for this process
- a "higher" layer, which by convention consumes services from this process
- an "environment", which by default provides very constrained functionality -
  a clock and a "black-hole" sink for logging.

This layer-based structure allows processes to be composed via stacking (to be
implemented later), to form a single composite process.

Finally, the interfaces may be hooked to impure (non-'Process') components, in
order to run it in the "real world". For example, the lower interface to a
networking runtime, the higher interface to a UI, and the environmental
interface to a clock. Or, these might be mock components that provide a
predictable feed of messages, e.g. for testing or debugging.

Typically, one defines this instance on the process state type @ps@.

Typically, the lower/lowest interface should conform to a protocol type that is
recognised by one of our provided runtimes. For example, {- workaround haddock
bug #1251 -} t'P4P.Proc.Protocol.UProtocol' or t'P4P.Proc.Stream.SProtocol'. Of
course, you may also write your own runtime that recognises your special lower
interface.
-}
class ProcIface ps where
  -- | Type of message from the lower layer to this process.
  type LoI ps :: Type
  -- | Type of message from this process to its lower layer.
  type LoO ps :: Type

  -- | Type of message from the higher layer to this process.
  type HiI ps :: Type
  -- | Type of message from this process to its higher layer.
  type HiO ps :: Type

  {- | Type of message from the environment to this process.

  This is always 'Tick' in our layer-based model; all other I/O is supposed to
  pass via the lower and higher layers.
  -}
  type EnvI ps :: Type
  type EnvI ps = Tick

  {- | Type of message from this process to the environment.

  This is always 'Tick' in our layer-based model; all other I/O is supposed to
  pass via the lower and higher layers.

  For 'Tick', this gives the process the ability to assert that the timing of
  when outgoing messages are emitted, is part of the contract of its behaviour.
  How this works is as follows: the process may output ticks to fix the order
  of outgoing messages relative to these tick outputs, and this will be
  expected to be reproducible when replayed.

  If the process does not output ticks, then it is only the relative ordering
  of other outgoing messages that are part of the contract of its behaviour.
  When replayed later, this ordering must still be reproducible, but the
  precise tick at which they are output is not checked.

  In the latter case, it is still recommended to leave this as 'Tick' for
  interoperability, unless you are composing your process with another one in
  such a way that requires this to be set to something else - the documentation
  for that composition function will give further instructions if necessary.
  -}
  type EnvO ps :: Type
  type EnvO ps = Tick

  {- | Type of auxiliary message from this process, e.g. logging messages.

  This is exempt from the deterministic behaviour contract.
  -}
  type AuxO ps :: Type
  type AuxO ps = Void

-- | Empty (unit) protocol, useful for composing with other protocols.
instance ProcIface () where
  type LoI () = Void
  type LoO () = Void
  type HiI () = Void
  type HiO () = Void

type GMsgI e l h a = GMsg 'Incoming e l h a
type GMsgO e l h a = GMsg 'Outgoing e l h a

-- | Process incoming message type.
type PMsgI ps = GMsgI (EnvI ps) (LoI ps) (HiI ps) Void
-- | Process outgoing message type.
type PMsgO ps = GMsgO (EnvO ps) (LoO ps) (HiO ps) (AuxO ps)
-- | Process outgoing message type, ignoring auxilliary output.
type PMsgO_ ps = GMsgO (EnvO ps) (LoO ps) (HiO ps) Void

{- | Pure communicating process.

If your state type implements this, then deriving an instance of 'Process'
becomes trivial - all default methods will work. See also 'Instances.hs' for
some automatic 'Process' instances of Lens/MutVars wrapped around the state.
-}
class ProcIface ps => Proc ps where
  -- | React to inputs.
  react :: PMsgI ps -> ps -> ([PMsgO ps], ps)

instance Proc () where
  react v () = ([], ())

{- | Communicating process.

First, see 'ProcIface' on our communication model for processes. Then:

Processes must be deterministic and interact with their environment only
via 'react'. In other words:

  * @pure state@ is equal to @'proceed' p state >> suspend p@.
  * @'suspend' p >>= proceed p@ must have no meaningful side-effects.
  * Any meaningful side-effects of @react@ must be fully-represented within
    @p@ as part of @'State' p@.

Equivalently, 'reactAllM' on any inputs must be deterministic in the outputs
and all side-effects of its computation context @m@ must be not meaningful.

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

A process is similar to the concept of an 'Control.Arrow.Arrow', but has the
following important differences:

- its I/O message types are fixed via the associated type families of
  @'ProcIface' ps@, by convention go to different components, and
  composition methods follow these conventions. For 'Control.Arrow.Arrow',
  its I/O message types do not have any structure, and composition methods
  have the responsibility of defining the structure to branch on. In other
  words, 'Control.Arrow.Arrow' is more general in this regard.

- it produces any number of outputs from a single input, whereas
  'Control.Arrow.Arrow' only produces one. In other words, 'Process' is more
  general in this regard.
-}
class ProcIface (State p) => Process p where
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

  -- | React to inputs.
  reactM :: Ctx p m => p -> ProcMsgI p -> m [ProcMsgO p]

  default reactM
    :: (Ctx p m, PureProcess p, Proc (State p))
    => p -> ProcMsgI p -> m [ProcMsgO p]
  reactM pt = runPure pt . react

-- | A process that can embed arbitrary pure computations over its state.
class Process p => PureProcess p where
  -- | Run a pure state-machine in the process.
  runPure :: Ctx p m => p -> (State p -> (a, State p)) -> m a

-- | Type alias for the messages of a process.
type ProcMsgI p = PMsgI (State p)
type ProcMsgO p = PMsgO (State p)

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

-- | Simple model of a process IO, for 'runReactProc' et. al.
data ProcIO ps m = ProcIO
  { procLog :: !(EnvI ps -> Either (PMsgI ps) [PMsgO ps] -> m ())
  -- ^ Log an input or several outputs.
  , procI   :: !(m (Maybe (PMsgI ps)))
  -- ^ Get an input, or EOF.
  , procO   :: !([PMsgO ps] -> m ())
  -- ^ Deal with a batch of outputs.
  -- Batching allows the env to perform flow control more pro-actively if
  -- needed, since it now also receives a signal on [].
  }

liftProcIO
  :: forall ps t m . (MonadTrans t, Monad m) => ProcIO ps m -> ProcIO ps (t m)
liftProcIO ProcIO {..} = ProcIO { procLog = (lift .) . procLog
                                , procI   = lift procI
                                , procO   = lift . procO
                                }

-- | React to inputs, and handle outputs, with the given actions.
runReactProc :: (Proc ps, Monad m) => ProcIO ps m -> EnvI ps -> ps -> m ps
runReactProc procIO ei0 ps0 = flip execStateT ps0 $ go ei0
 where
  ProcIO {..} = liftProcIO procIO
  go ei = procI >>= \case
    Nothing -> pure ()
    Just i  -> do
      procLog ei $ Left i
      outs <- state $ react i
      procLog ei $ Right outs
      procO outs
      go $! case i of
        MsgEnv ei' -> ei'
        _          -> ei

-- | React to inputs, and handle outputs, with the given actions.
runReactProcess
  :: (Process p, Monad m, Ctx p m)
  => ProcIO (State p) m
  -> EnvI (State p)
  -> p
  -> m ()
runReactProcess procIO ei0 proc = go ei0
 where
  ProcIO {..} = procIO
  go ei = procI >>= \case
    Nothing -> pure ()
    Just i  -> do
      procLog ei $ Left i
      outs <- reactM proc i
      procLog ei $ Right outs
      procO outs
      go $! case i of
        MsgEnv ei' -> ei'
        _          -> ei

-- | 'runReactProcess' with a particular input state, and return an output state.
runReactProcess'
  :: forall p m
   . (Process p, Monad m, Ctx p m)
  => ProcIO (State p) m
  -> EnvI (State p)
  -> State p
  -> m (State p)
runReactProcess' procIO ei0 ps0 =
  snd <$> asState (runReactProcess @p procIO ei0) ps0
