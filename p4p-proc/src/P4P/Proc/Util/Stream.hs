{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{- | Utilities and abstractions for linearly-ordered flow-controlled streams.

A plain 'P4P.Proc.Proc' supports datagram-oriented IO, without any guarantees
on how the messages are delivered. In the real world, endpoints need to
co-operate in order to avoid congestion; this is called flow-control which
typically also brings some guarantees on ordering.

In this model we first present a simple abstract model of linearly-ordered
flow-controlled streams between two peers, which can be used as the "input"
and "output" types of a 'P4P.Proc.Proc' that you can then hook into a runtime
that executes the messages over a real protocol capable of realising this
abstraction, such as QUIC - see @p4p-rt-quic@ for more details.

-}
module P4P.Proc.Util.Stream where

-- external
import           Codec.Serialise (Serialise)
import           Data.Binary     (Binary)
import           Data.Sequence   (Seq)
import           Data.Word
import           GHC.Generics    (Generic)


{- | The direction of a stream. Our streams are uni-directional. -}
data StreamDirection = StreamIncoming | StreamOutgoing
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

{- | A stream id. This is a 56-bit integer, to fit into a QUIC stream id.

A stream id is used to help identify a stream. Specifically, the 3-tuple
@('StreamDirection', 'StreamId', peerid)@ fully identifies a logical stream in
the context of a running instance of 'P4P.Proc.Proc'.

That is, the same 'StreamId' may be used for two streams - an outgoing and an
incoming one. They are logically separate streams - their ordering and flow
control is handled separately - but they can be thought of as parts of a single
bidirectional stream for convenience.

The details of how this is mapped into a QUIC stream id are not important for
users, but you can read about it in the documentation of @p4p-rt-quic@.
-}
data StreamId = StreamId !Word8 !Word16 !Word32
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise, Bounded)

type StreamFullId id = (StreamDirection, StreamId, id)

{- | Stream input. See also 'StreamO'. -}
data StreamI id addr imsg =
    SIRecv !StreamId !id !imsg
    -- ^ Received a message on the given incoming stream.
  | SOReady !StreamId !id
    -- ^ We are ready to send another message on the given outgoing stream.
  | SOwnAddrs !(Seq addr)
    -- ^ Tell me my own addresses, so I can e.g. publish them somewhere
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

{- | Stream output.

Our API focuses on convenience - under normal usage, streams are opened and
closed implicitly by the runtime; one does not have to explicitly do this nor
worry about whether a stream is open or closed.

Under certain special circumstances one can explicitly:

- open an incoming stream, by outputting @'SIAccept' _ _ True@
- open an outgoing stream, by outputting @'SOSend' _ _ Nothing@, thereby adding
  this complexity to the type parameter @omsg@.
- close all streams for a peer, by outputting @'SIAllow' _ False@.

TODO: add bandwidth-limiting commands.
-}
data StreamO id omsg =
    SOSend !StreamId !id !omsg
    {- ^ Send a message on the given outgoing stream.

    Either this is a new stream (i.e. we have previously not sent a message on
    this stream), or we have received a 'SOReady' for this stream after any
    previously-sent message.

    If these preconditions are not met, the runtime may panic or throw an
    error. 'P4P.Proc.Util.FlowControl' contains utilities to ensure these
    preconditions are met.
    -}
  | SIAccept !StreamId !id !Bool
    {- ^ Whether to receive messages on the given stream.

    This starts off being @True@ for all streams. However if the underlying
    runtime does not already have a stream open, sending an explicit @True@
    tells it to attempt to open the stream, i.e. to contact the peer on the
    network. Sometimes this helps to bypass NAT or other network restrictions,
    in case the peer cannot initiate contact with us but we can them.

    Setting this to @False@ just stops any future reads of the stream, but does
    not explicitly close the stream. The runtime may close them after a certain
    timeout, to maintain a balance between avoiding reconnection overhead vs
    long-term memory usage of many many open streams. Note that QUIC streams
    are lightweight so the latter is typically not a concern.
    -}
  | SIAllow !id !Bool
    {- ^ Whether to receive messages on any stream of the given peer.

    This starts off being @True@ for all peers.

    When set to @False@, the runtime will close all underlying streams, and
    free all resources associated with the peer, except a minimal amount needed
    to prevent the peer reconnecting. This can be used e.g. to block a peer
    that was misbehaving, or simply to stop talking to a peer because you chose
    another as a replacement.

    When set to @True@, the above will be forgotten. No streams will be
    reopened at that instant, but future attempts to reopen them will no longer
    be blocked.
    -}
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)
