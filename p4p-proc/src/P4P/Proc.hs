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
  -- proc
  , Proc(..)
  , Process(..)
  , ProcAddr
  , ProcMsgI
  , ProcMsgO
  , reactAllM
  , purely
  -- instances
  --, SL(..)
  , MV(..)
  )
where

import           P4P.Proc.Instances
import           P4P.Proc.Internal
