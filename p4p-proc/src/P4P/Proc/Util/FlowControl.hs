{-# LANGUAGE TypeFamilies #-}

{- | Utilities for performing flow control.

Flow control for sending /to/ peers, is controlled by your peer, and you must
respond to /their/ signals.

Basically, your peer will send you signals whenever they are ready to accept
more data and you should not send more unless you receive these signals. In the
system API these signals could be represented as an explicit ACK message, or
more typically as with TCP sockets, it is represented implicitly as the
previous blocking call to @send()@ unblocking.

Either way, this should be translated into a call to 'receiverReady' below.
Separately, whenever your main logic has extra stuff to send, you do not send
it directly, but rather use 'maybeSend'.

Flow control for receiving /from/ peers is controlled by you, and is divided
into two parts:

1. deciding when you need to trigger flow control
2. actually performing the flow control

For (1) there are two approaches - (a) setting concrete resource limits, and
(b) automatically detecting when your machine is hitting its resource limit.

For CPU resources, (b) is the more natural approach. Typically inputs come in
on a buffer, and when they become full you are likely to be processing at your
maximum speed already, and can drop new items attempting to overflow the buffer
and/or NACK them. Note this must be done in impure code, since by the time the
pure code is executing, the input has already been popped from the buffer.

For memory resources, this is a bit harder. In principle, you can measure the
space used by the state machine (or the additional space used by the processing
of one input), and NACK incoming inputs until the usage is within limit. In
practise, this is hard to do accurately in Haskell. This should be explored
further; for now we suggest the inaccurate but simple method just described,
i.e. attempting to measure the size of your state. This would at least force
the evaluation of any thunks.

For (2), due to the arrangement of the data flow, your impure code naturally is
going to be the final enforcer of the decision, but you may want it to be
guided by signals from the pure code. For example, TCP APIs do not let you send
explicit ACKs, and so the enforcement must be done in impure code, i.e. that it
stops reading from the TCP socket and therefore implicitly blocks the sender.
On the other hand, if your pure code wants to have a say in how this is
enforced (e.g. because part of its state is growing too big), it will need to
give explicit signals to the impure code to do this, rather than merely relying
on the implicit signal of "an input was processed".

Note that this module currently does not have any special utilies to handle
receiving flow control; but this might change in future.
-}
module P4P.Proc.Util.FlowControl where

{- | Flow control structures for a given recipient.

To control multiple recipients, call the below functions but using different
instances of this data structure. For example you could have a map of these
per recipient, and use lens to access elements of the map.

You should do this even if sending the same message to multiple recipients,
since everyone needs to be flow-controlled separately.
-}
type FlowControl queue = (Bool, queue)

{- | Maybe send a message, to some implied recipient.

Push a message onto the queue, and if the recipient is currently waiting for a
message, pop a message from the queue, send it, and unmark the waiting flag.
-}
maybeSend
  :: (msg -> queue -> queue)
  -> (queue -> (Maybe msg, queue))
  -> msg
  -> FlowControl queue
  -> (Maybe msg, FlowControl queue)
maybeSend push maybePop msg (w0, q0) = if w0
  then
    let (msg', q1) = maybePop $ push msg q0
    in  case msg' of
          Nothing -> (msg', (w0, q1))
          Just _  -> (msg', (False, q1))
  else (Nothing, (w0, push msg q0))

{- | A receiver is now ready to receive another message.

If the queue is non-empty, pop a message and send it. Otherwise, mark the
recipient as "waiting".

@Left ()@ means that the receiver was already marked as ready. This usually
means some incorrect logic in the calling code, probably that it did not
actually send a message after the previous 'receiverReady'.
-}
receiverReady
  :: (queue -> (Maybe msg, queue))
  -> FlowControl queue
  -> (Either () (Maybe msg), FlowControl queue)
receiverReady maybePop (w0, q0) = if w0
  then (Left (), (w0, q0))
  else
    let (msg', q1) = maybePop q0
    in  case msg' of
          Nothing -> (Right msg', (True, q1))
          Just _  -> (Right msg', (w0, q1))

-- | A queue-like data structure for storing flow-controlled outgoing messages.
class FlowControlQueue queue where
  type FCMsg queue

  -- | Pop the most urgent message from the queue, or 'Nothing' if empty.
  maybePopQ :: queue -> (Maybe (FCMsg queue), queue)

  -- | Push a new message onto the queue, possibly evicting items that are made
  -- obsolete by the new message.
  pushQ :: FCMsg queue -> queue -> queue

-- | 'maybeSend' but with a constraint instead of explicit functions.
maybeSendQ
  :: FlowControlQueue q
  => FCMsg q
  -> FlowControl q
  -> (Maybe (FCMsg q), FlowControl q)
maybeSendQ = maybeSend pushQ maybePopQ
{-# INLINE maybeSendQ #-}

-- | 'receiverReady' but with a constraint instead of explicit functions.
receiverReadyQ
  :: FlowControlQueue q
  => FlowControl q
  -> (Either () (Maybe (FCMsg q)), FlowControl q)
receiverReadyQ = receiverReady maybePopQ
{-# INLINE receiverReadyQ #-}
