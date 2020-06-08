{-| P4P simulations. -}

module P4P.Sim
  (
  -- * Main definitions
    KV(..)
  , Pid
  , SimUserI'(..)
  , SimUserI
  , SimUserO'(..)
  , SimUserO
  , SimProcEvt'(..)
  , SimProcEvt
  , SimState'(..)
  , SimState
  , SimError(..)
  , NonEmpty(..)
  , newSimState
  , SimRT(..)
  , SimT
  , SimProcess
  , proceedAll
  , suspendAll
  , simulate
  , runSim
  -- * Extensions
  , SimXProtocol(..)
  , runSimX
  , runSimXS
  -- * Options
  , SimOptions(..)
  , parseArgsIO
  , parserInfo
  , simOptions
  , SimProto(..)
  -- * IO and utilities
  , SimLog
  , SimReRe
  , defaultSimUserIO
  , grunSimIO
  , runSimIO
  , handleSimResult
  , UserSimAsync(..)
  , newSimAsync
  )
where

import           Data.List.NonEmpty (NonEmpty (..))

import           P4P.Sim.Extension
import           P4P.Sim.Internal
import           P4P.Sim.IO
import           P4P.Sim.Options
import           P4P.Sim.Types
