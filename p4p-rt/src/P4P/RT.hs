{-| P4P runtime library.

This package implements concrete runtime behaviour for "P4P.Proc", including
suspend-and-resume, record-and-replay, console UI, and UDP networking.

== Crash-proof properties of the RT

The deterministic design of p4p gives us a basic level of crash-proof behaviour
- if you record the input state and input messages, after any crashes you can
simply replay this to get the resulting output messages and state, upon which
you can then resume running on real-world input messages again.

Due to the nature of real-world computing, after a crash it is possible for the
final few messages to be lost or malformed beyond readability at the end of a
recorded stream; this is unavoidable regardless of how well we design p4p.
Robust protocols can and are designed to handle these situations - for example
the peer should resend the message if it was not acknowledged within a timeout.
In other words, after a crash it should be fine to truncate any final malformed
messages before performing the resumption; the protocol will recover by itself.

Nevertheless we do have plans to continually tighten up these aspects of the
runtime, so that e.g. fewer messages have to be replayed after a crash before
being able to resume correctly, messages are lost with less probability, and
the resumption-recovery process described above will be automated (it is
already possible to it manually). See the TODO for details.
-}
module P4P.RT
  ( module P4P.RT.Options
  , RTError(..)
  , RTHiIO
  , RTLoIO
  , runProcIO
  , runProcIO'
  , defaultRTLogging
  , handleRTResult
  , convertProcData
  , XTHiM
  , XTHiIO
  , RTAsync(..)
  , newRTAsync
  -- client
  , StdIO
  , defaultRTHiIO
  , defaultRTHiIO'
  , KV(..)
  , bracket2
  , onExceptionShow
  , optionTerminalStdIO
  , initializeTick
  , initializeTickAddrs
  )
where

import           P4P.RT.Client
import           P4P.RT.Internal
import           P4P.RT.Options
