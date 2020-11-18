{-| P4P process abstractions. -}

module P4P.Proc
  ( ProcIface(..)
  , GMsg(..)
  , GMsgI
  , GMsgO
  , PMsgI
  , PMsgO
  , PMsgO'
  , Proc(..)
  , Process(..)
  , ProcMsgI
  , ProcMsgO
  , reactAllM
  , asState
  , ProcEnv(..)
  , liftEnv
  , envReactProc
  , envReactProcess
  , envReactProcess'
  , Tick
  -- protocol
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
  , UProtocol(..)
  , UMsg(..)
  , UMsgI
  , UMsgO
  , UPMsgI
  , UPMsgO
  , ProcAddr
  -- instances
  , PRef(..)
  , PMut(..)
  )
where

import           Data.Schedule      (Tick)
import           P4P.Proc.Instances
import           P4P.Proc.Internal
import           P4P.Proc.Protocol
