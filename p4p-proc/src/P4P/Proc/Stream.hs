{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{- | Processes that communicate via secure linearly-ordered flow-controlled streams.

A plain 'P4P.Proc.Proc' communicates via datagram-oriented IO, without any
guarantees on how the messages are delivered. In the real world, endpoints need
to co-operate in order to avoid congestion; this is called flow-control which
typically also brings some guarantees on ordering. For extras, we also throw in
transport-level security, which also lets us unique identify protocol entities.

In this module, we (1) present a simple abstract model of linearly-ordered
flow-controlled streams between two peers, then (2) use this to augment our
basic 'P4P.Proc.Proc' as 'SProc'. 'SProc' can be run on an enriched runtime
that supports it, such as @p4p-rt-quic@.

This is not an ideal solution that adheres to the principles specified in our
README. Ideally, we'd implement QUIC as a pure @Proc@ that:

- presents our stream-based API (as defined by 'SRuntimeI' and 'SRuntimeO') to
  its higher layer client (which would be another @Proc@) via 'UserI'/'UserO'
  messages.
- can be run on a much simpler UDP-based runtime, such as @p4p-rt@.

However implementing QUIC in pure logic would be quite a big undertaking, and
we would like to spend our time on other things at present.
-}
module P4P.Proc.Stream where

-- external
import           Codec.Serialise (Serialise)
import           Data.Binary     (Binary)
import           Data.Kind       (Type)
import           Data.Schedule   (Tick)
import           Data.Void       (Void, absurd)
import           Data.Word
import           GHC.Generics    (Generic)

-- internal
import           P4P.Proc.Types


{- | The direction of a stream. Our streams are uni-directional. -}
data StreamDirection = StreamIncoming | StreamOutgoing
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

{- | A stream id. This is a 56-bit integer, to fit into a QUIC stream id.

A stream id is used to help identify a stream. Specifically, the 3-tuple
@(peerid, 'StreamDirection', 'StreamId')@ fully identifies a logical stream in
the context of a running process.

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
type StreamFullId pid = (pid, StreamDirection, StreamId)

{- | Runtime input for stream-capable protocols. -}
data SRuntimeI pid addr =
    SRTTick !Tick
    -- ^ Time has passed into a new tick.
  | SRTOReady !pid !StreamId
    -- ^ We are ready to send another message on the given outgoing stream.
  | SRTRAddrs !pid !(Observations addr)
    {- ^ The runtime informs the process about id-address mappings.

    This is based on the runtime's observations at its lower layer, e.g.
    observing a response from a network id.
    -}
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

{- | Runtime output for stream-capable protocols.

Our API focuses on convenience - under normal usage, streams are opened and
closed implicitly by the runtime; one does not have to explicitly do this nor
worry about whether a stream is open or closed.

Under certain circumstances we do provide the ability to tell the runtime to
open or close a stream; read below for details.

TODO: add bandwidth-limiting commands.
-}
data SRuntimeO pid addr =
    SRTPAddrs !pid !(Observations addr)
    {- ^ The process informs the runtime about id-address mappings.

    This is based on the protocol's observations at its higher layer, e.g.
    observing a signed response from other network peers.
    -}
  | SRTIAccept !pid !StreamId !Bool
    {- ^ Whether to receive messages on the given incoming stream.

    This starts off being @True@ for all streams. However if the runtime does
    not already have a stream open, sending an explicit @True@ tells it to
    attempt to open the stream, i.e. to contact the peer on the network.
    Sometimes this helps to bypass NAT or other network restrictions, in case
    the peer cannot initiate contact with us but we can them.

    Setting this to @False@ just stops any future reads of the stream, but does
    not explicitly close the stream. The runtime may close them after a certain
    timeout, to maintain a balance between avoiding reconnection overhead vs
    long-term memory usage of many many open streams. Note that QUIC streams
    are lightweight so the latter is typically not a concern.
    -}
  | SRTIAllow !pid !Bool
    {- ^ Whether to receive messages on any stream of the given peer.

    This starts off being @True@ for all peers.

    When set to @False@, the runtime immediately closes the streams, and frees
    all resources associated with the peer, except a minimal amount needed to
    prevent the peer from reconnecting. This can be used e.g. to block a peer
    that was misbehaving, or simply to stop talking to a peer because you chose
    another as a replacement.

    When set to @True@, the above will be forgotten. No streams are reopened
    immediately, but future attempts to reopen them will no longer be blocked.
    -}
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)


class ProtoMsg msg => SProtoMsg msg where
  streamId :: msg -> StreamId

instance SProtoMsg Void where
  streamId = absurd

{- | Stream-capable protocol.

The protocol entity ('Ent') here is a cryptographic id that uniquely identifies
the entity. However it contains no information about their address, so we have
an extra type to provide this information.
-}
class (SProtoMsg (PMsg ps), Protocol ps) => SProtocol ps where
  type SAddr ps :: Type

instance SProtocol () where
  type SAddr () = Void

{- | Stream-capable process.

This is an augmented 'P4P.Proc.Proc' that uses secure linearly-ordered
flow-controlled streams for communication with other processes. It must be run
on an enriched runtime that supports this such as @p4p-rt-quic@, not @p4p-rt@.
-}
class SProtocol ps => SProc ps where

  {- | React to inputs.

  Since this is a flow-controlled protocol, we may not send outgoing messages
  arbitrarily to other processes. We may only send a message @'MsgProc' msg@ on
  a given stream @s := ('getTarget' msg, 'StreamOutgoing', 'streamId' msg)@ if:

  - @s@ is a new stream i.e. we have not previously-sent a message on @s@, or
  - we have received a 'SRTOReady' for @s@ after any previously-sent message on @s@

  If these preconditions are not met, the runtime may panic or throw an error.
  'P4P.Proc.Util.FlowControl' contains utilities to ensure these are met.
  -}
  react :: SGMsgI ps -> ps -> ([SGMsgO ps], ps)

instance SProc () where
  react v () = ([], ())

type SGMsgI ps
  = GMsg (SRuntimeI (PEnt ps) (SAddr ps)) (UserI ps) (PMsg ps) Void
type SGMsgO ps
  = GMsg (SRuntimeO (PEnt ps) (SAddr ps)) (UserO ps) (PMsg ps) (AuxO ps)
