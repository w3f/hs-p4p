{-| P4P process abstractions. -}

module P4P.Proc
  ( ProtoMsg(..)
  , RuntimeI(..)
  , RuntimeO(..)
  , Protocol(..)
  , PAddr
  , GMsg(..)
  , GMsgI
  , GMsgO
  , Void
  -- proc
  , Proc(..)
  , Process(..)
  , ProcAddr
  , ProcMsgI
  , ProcMsgO
  , reactAllM
  , purely
  -- instances
  , PRef(..)
  , PMut(..)
  )
where

import           Data.Void          (Void)
import           P4P.Proc.Instances
import           P4P.Proc.Internal
