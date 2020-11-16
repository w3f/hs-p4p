{-| P4P simulations. -}

module P4P.Sim
  (
  -- * Main definitions
    KV(..)
  , Pid
  , SimHiI'(..)
  , SimHiI
  , SimHiO'(..)
  , SimHiO
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
  , convertSimData
  -- * Extensions
  , SimXProcIface(..)
  , runSimX
  , runSimXS
  -- * Options
  , SimXOptions(..)
  , SimOptions(..)
  , mkParser
  , parseArgsIO
  , parseArgsIO'
  , simXOptions
  , simOptions
  , SimConvOptions(..)
  , simConvOptions
  , module Options.Applicative
  , showOptions
  , allOptions
  -- * IO and utilities
  , SimLog
  , SimUserRe
  , SimReReP
  , SimReReX
  , SimReRe
  , defaultSimUserIO
  , defaultStdIO
  , grunSimIO
  , runSimIO
  , handleSimResult
  , UserSimAsync(..)
  , newSimAsync
  )
where

import           Data.List.NonEmpty  (NonEmpty (..))

import           Options.Applicative
import           P4P.Sim.Extension
import           P4P.Sim.Internal
import           P4P.Sim.IO
import           P4P.Sim.Options
import           P4P.Sim.Types
