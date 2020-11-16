{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{- | Secure linearly-ordered flow-controlled streams.

Our default protocol type t'P4P.Proc.Protocol.UProtocol' is for unordered
unreliable datagram-oriented IO, without any guarantees on how the messages are
delivered. In the real world, endpoints need to co-operate in order to avoid
congestion; this is called flow-control which typically also brings some
guarantees on ordering. For extras, we also throw in transport-level security,
which also lets us uniquely identify protocol entities.

To this end, here we present another protocol type 'SProtocol', a simple
abstract model of linearly-ordered flow-controlled streams between two peers.
A t'P4P.Proc.Process' can then use this for its lower interface as required.

Such a process can be run on an enriched runtime that supports 'SProtocol',
such as @p4p-rt-quic@. Note that this is not an ideal solution that adheres to
the principles specified in our README. Ideally, we'd implement QUIC as a pure
@Proc@ that

- uses t'P4P.Proc.Protocol.UProtocol' as its lower interface, and
- presents 'SProtocol' as its higher interface.

Higher-layer components can then sit on top of this, and the whole stack can be
implemented on our much simpler UDP-based runtime, such as @p4p-rt@. However
implementing QUIC in pure logic would be quite a big undertaking, and we would
like to spend our time on other things at present - so @p4p-rt-quic@ is a
temporary compromise solution.
-}
module P4P.Proc.Stream where

-- external
import           Codec.Serialise   (Serialise)
import           Data.Binary       (Binary)
import           Data.Kind         (Type)
import           Data.Word
import           GHC.Generics      (Generic)

-- internal
import           P4P.Proc.Internal (Direction (..))
import           P4P.Proc.Protocol (Observations)


{- | A stream id. This is a 56-bit integer, to fit into a QUIC stream id.

A stream id is used to help identify a stream. Specifically, the 3-tuple
@(peerid, 'Direction', 'StreamId')@ fully identifies a logical
stream in the context of a running process.

That is, the same 'StreamId' may be used for two streams - an outgoing and an
incoming one. They are logically separate streams - their ordering and flow
control is handled separately - but they can be thought of as parts of a single
bidirectional stream for convenience.

The details of how this is mapped into a QUIC stream id are not important for
users, but you can read about it in the documentation of @p4p-rt-quic@.
-}
data StreamId = StreamId !Word8 !Word16 !Word32
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise, Bounded)

{- | Convenience type alias.

Fully identifies a stream in the context of a process.
-}
type StreamFullId pid = (pid, Direction, StreamId)

{- | Stream-capable protocol.

Our API focuses on convenience - under normal usage, streams are opened and
closed implicitly by the lower layer; one does not have to explicitly do this
nor worry about whether a stream is open or closed.

OTOH, under certain circumstances we do provide the ability to tell the lower
layer to open or close a stream if really needed; see 'SMsgO' for details.

When defining this instance for your @$ps@, you should also define:

@
instance t'P4P.Proc.ProcIface' $ps where
  type LoI $ps = 'SPMsgI' $ps
  type LoO $ps = 'SPMsgO' $ps
  -- remaining defs ..
@

See 'SMsgI' and 'SMsgO' for more details.
-}
class SProtocol ps where
  type Addr ps :: Type
  -- | A cryptographic id that uniquely identifies an entity.
  type Pid ps :: Type
  -- | Main protocol message type, for external communication between entities.
  type Msg ps :: Type

-- | Local message that encodes incoming actions within 'SProtocol'.
data SMsgI pid addr msg =
    SIAddrs !pid !(Observations addr)
    {- ^ The lower layer informs the process about id-address mappings.

    This is based on the lower layer's observations, e.g. observing a response
    from a network address.

    If @pid@ is our own identity, then this has the same semantics as
    t'P4P.Proc.Protocol.UOwnAddr' in the 'Incoming' context. (We assume that
    the lower layer knows our own identity via out-of-band means, e.g. during
    initialisation.)
    -}
  | SIRecv !pid !StreamId !msg
    -- ^ We received a message on a stream.
  | SOReady !pid !StreamId
    -- ^ We are ready to send another message on the given outgoing stream.
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

{- | Local message that encodes outgoing actions within 'SProtocol'.

TODO: add bandwidth-limiting commands.
-}
data SMsgO pid addr msg =
    SOAddrs !pid !(Observations addr)
    {- ^ The process informs the lower layer about id-address mappings.

    This is based on the protocol's observations at its higher layer, e.g.
    observing a signed response from other network peers.

    If @pid@ is our own identity, then this has the same semantics as
    t'P4P.Proc.Protocol.UOwnAddr' in the 'Outgoing' context. (We assume that
    the lower layer knows our own identity via out-of-band means, e.g. during
    initialisation.)
    -}
  | SOSend !pid !StreamId !msg
    {- ^ Send a message on a stream.

    Since this is a flow-controlled protocol, we may not send outgoing messages
    arbitrarily to other processes. We may only send a message @'SOSend' pid
    sid msg@ on a given stream @s := (pid, 'Outgoing', sid)@ if:

    - @s@ is a new stream i.e. we have not previously-sent a message on @s@, or
    - we have received a 'SOReady' for @s@ after any previously-sent message on @s@

    If these preconditions are not met, the runtime may panic or throw an
    error. t'P4P.Proc.Util.FlowControl' contains utils to ensure these are met.
    -}
  | SIAccept !pid !StreamId !Bool
    {- ^ Whether to receive messages on the given incoming stream.

    This starts off being @True@ for all streams. However if the lower layer
    does not already have a stream open, sending an explicit @True@ tells it to
    attempt to open the stream, i.e. to contact the peer on the network.
    Sometimes this helps to bypass NAT or other network restrictions, in case
    the peer cannot initiate contact with us but we can them.

    Setting this to @False@ just stops any future reads of the stream, but does
    not explicitly close the stream. The lower layer may close them after some
    timeout, to maintain a balance between avoiding reconnection overhead vs
    long-term memory usage of many many open streams. Note that QUIC streams
    are lightweight so the latter is typically not a concern.
    -}
  | SIAllow !pid !Bool
    {- ^ Whether to receive messages on any stream of the given peer.

    This starts off being @True@ for all peers.

    When set to @False@, the lower layer immediately closes the streams, and
    frees all resources associated with the peer, except a minimal amount
    needed to prevent the peer from reconnecting. This can be used e.g. to
    block a peer that was misbehaving, or simply to stop talking to a peer
    because you chose another as a replacement.

    When set to @True@, the above will be forgotten. No streams are reopened
    immediately, but future attempts to reopen them will no longer be blocked.
    -}
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

type SPMsgI ps = SMsgI (Pid ps) (Addr ps) (Msg ps)
type SPMsgO ps = SMsgO (Pid ps) (Addr ps) (Msg ps)
