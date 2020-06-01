{-| P4P simulations. -}

module P4P.Sim
  (
  -- * Main definitions
    KV(..)
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
  , Sim
  , proceedAll
  , suspendAll
  , simulate
  , runSimulation
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
  , runSimIO
  , handleSimResult
  , UserSimAsync(..)
  , newSimAsync
  )
where

import           Data.List.NonEmpty (NonEmpty (..))

import           P4P.Sim.Internal
import           P4P.Sim.IO
import           P4P.Sim.Options
import           P4P.Sim.Types
