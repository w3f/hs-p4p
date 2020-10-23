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
  , convertSimData
  -- * Extensions
  , SimXProtocol(..)
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
