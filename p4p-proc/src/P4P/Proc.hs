{-| P4P process abstractions. -}

module P4P.Proc
  ( ProcIface(..)
  , GMsg(..)
  , GMsgI
  , GMsgO
  , PMsgI
  , PMsgO
  , Proc(..)
  , Process(..)
  , ProcMsgI
  , ProcMsgO
  , reactAllM
  , asState
  , ProcEnv(..)
  , reactEnv
  -- protocol
  , Observation(..)
  , Observations
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

import           P4P.Proc.Instances
import           P4P.Proc.Internal
import           P4P.Proc.Protocol
