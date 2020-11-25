{-| P4P process abstractions. -}

module P4P.Proc
  ( -- re-exports
    Void
  , Tick
  -- general
  , PortNumber
  , SockAddr(..)
  , Observation(..)
  , obsToPair
  , obsIsPositive
  , obsIsNegative
  , Observations
  , obsPositiveFromList
  , obsPositiveToSet
  , updateTrustedObs
  , ExtVal(..)
  , Codec(..)
  , cborCodec16
  , ProtocolCodecError(..)
  -- process
  , Direction(..)
  , GMsg(..)
  , _MsgEnv
  , ProcIface(..)
  , GMsgI
  , GMsgO
  , PMsgI
  , PMsgO
  , PMsgO_
  , Proc(..)
  , Process(..)
  , ProcMsgI
  , ProcMsgO
  , reactAllM
  , asState
  , ProcIO(..)
  , liftProcIO
  , runReactProc
  , runReactProcess
  , runReactProcess'
  -- protocol
  , UMsg(..)
  , UProtocol(..)
  , UMsgI
  , UMsgO
  , UPMsgI
  , UPMsgO
  , PMsgI'
  , PMsgO'
  , PMsgO_'
  , ProcAddr
  , withCodec
  , withCodecF
  -- instances
  , PRef(..)
  , PMut(..)
  )
where

import           Data.Schedule      (Tick)
import           Data.Void          (Void)
import           P4P.Proc.Instances
import           P4P.Proc.Internal
import           P4P.Proc.Protocol
import           P4P.Proc.Types
