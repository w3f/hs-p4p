{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- | Higher-kinded processes.

TODO: this is a WIP, not ready yet

Each process has a tag so you can distinguish between different types of
processes, e.g. if you have a client-server architecture or something.

By design, there are adapters from this to the non-higher-kinded processes, and
our process simulators deal only with the latter - since this type-level
information is not available when dealing with potentially malicious processes
and when simulating arbitrary processes.

However the extra type-level annotations could be useful for (honest) protocol
implementors, and makes it easy to e.g. ensure that one's code is only dealing
with correctly-typed messages.

The "K" prefix stands for "kinded".
-}
module P4P.Proc.Internal1 where

-- external
import           Data.Kind         (Constraint, Type)
import           Data.Type.Higher  (U1 (..))

-- internal
import           P4P.Proc.Internal


-- | Protocol with its protocol-messages and environment-messages.
class ProtoMsg (U1 (KPMsg ps)) => KProtocol tk (ps :: tk -> Type) | ps -> tk where
  -- | Protocol message type.
  type KPMsg ps :: (tk, tk) -> Type
  -- | Type of message from the user to a process.
  type KUserI ps :: tk -> Type
  -- | Type of message from a process to the user.
  type KUserO ps :: tk -> Type

type KPAddr ps = Addr (U1 (KPMsg ps))

data KGMsgI ps (t :: k) where
  MsgRTI :: !(RuntimeI ()) -> KGMsgI ps t
  MsgUserI :: !(KUserI ps t) -> KGMsgI ps t
  MsgProcI :: !(KPMsg ps '(s, t)) -> KGMsgI ps t -- TODO: we may want SingI s =>

data KGMsgO ps (s :: k) where
  MsgRTO :: !(RuntimeO (Addr (U1 (KPMsg ps)))) -> KGMsgO ps s
  MsgUserO :: !(KUserO ps s) -> KGMsgO ps s
  MsgProcO :: !(KPMsg ps '(s, t)) -> KGMsgO ps s -- TODO: we may want SingI t =>

class KProtocol tk ps => KProc tk (ps :: tk -> Type) | ps -> tk where

  -- | Receive-address(es) of a process.
  kGetAddrs :: ps t -> [Addr (U1 (KPMsg ps))]

  -- | React to inputs.
  kReact
    :: KGMsgI ps t -> ps t
    -> ([KGMsgO ps t], ps t)

{- | Communicating process. -}
class KProtocol tk (KState p) => KProcess tk (p :: tk -> Type) | p -> tk where
  -- | Type of process state
  type KState p :: tk -> Type
  -- | Constraint over execution environment type, e.g. 'Monad'.
  type KCtx p (m :: Type -> Type) :: Constraint

  kProceed :: KCtx p m => p t -> KState p t -> m ()

  kSuspend :: KCtx p m => p t -> m (KState p t)

  -- | Receive-address(es) of a process.
  kGetAddrsM :: KCtx p m => p t -> m [KProcAddr p]

  kReactM :: KCtx p m => p t -> KProcMsgI p t -> m [KProcMsgO p t]

  default kGetAddrsM
    :: (KCtx p m, KProc tk (KState p), Functor m)
    => p t -> m [KProcAddr p]
  kGetAddrsM = fmap kGetAddrs . kSuspend

  default kReactM
    :: (KCtx p m, KProc tk (KState p), Monad m)
    => p t -> KProcMsgI p t -> m [KProcMsgO p t]
  kReactM pt i = do
    s <- kSuspend pt
    let (o, s') = kReact i s
    kProceed pt s'
    pure o

-- | Type alias for the address of a process.
type KProcAddr p = KPAddr (KState p)

-- | Type alias for the messages of a process.
type KProcMsgI p = KGMsgI (KState p)
type KProcMsgO p = KGMsgO (KState p)


-- adapters between this and Internal0

instance KProtocol tk ps => Protocol (U1 ps) where
  type PMsg (U1 ps) = U1 (KPMsg ps)
  type UserI (U1 ps) = U1 (KUserI ps)
  type UserO (U1 ps) = U1 (KUserO ps)

instance KProc tk ps => Proc (U1 ps) where
  getAddrs = undefined
-- TODO: it may not actually be possible to write this as-is, but adding
-- Dynamic/SingI might help
  react    = undefined

instance KProcess tk ps => Process (U1 ps) where
  type State (U1 ps) = U1 (KState ps)
  type Ctx (U1 ps) m = KCtx ps m
  proceed   = undefined
  suspend   = undefined
  getAddrsM = undefined
  reactM    = undefined


-- below here is tests to make sure stuff is actually instantiable

data MyProcMsg (t :: (Bool, Bool)) where
  C2S :: MyProcMsg '( 'True, 'False)
  S2C :: MyProcMsg '( 'False, 'True)

instance ProtoMsg (U1 MyProcMsg) where
  type Addr (U1 MyProcMsg) = Int
  getTarget (U1 msg) = case msg of
    C2S -> 0
    S2C -> 1
  setSource = undefined

data MyProcState t = MyProcState

instance KProtocol Bool MyProcState where
  type KPMsg MyProcState = MyProcMsg
  type KUserI MyProcState = MyProcState
  type KUserO MyProcState = MyProcState
