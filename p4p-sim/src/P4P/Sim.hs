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
  , SimFullState
  , NonEmpty(..)
  , newSimState
  , SimT
  , SimProcess
  , simulate
  , runSim
  -- * Extensions
  , SimXProcIface(..)
  , runSimX
  , runSimXS
  -- * Options
  , SimXOptions(..)
  , SimOptions(..)
  , _simRTOptions
  , mkParser
  , parseArgsIO
  , parseArgsIO'
  , simXOptions
  , simOptions
  , module Options.Applicative
  , module P4P.RT.Options
  -- * IO and utilities
  , SimLog
  , SimUserRe
  , SimReReP
  , SimReReX
  , SimReRe
  , defaultSimUserIO
  , grunSimIO
  , runSimIO
  -- * RT generic utils
  , handleRTResult
  , convertProcData
  , RTAsync(..)
  , newRTAsync
  , bracketHEF
  , hookAutoJoinQuit
  , optionTerminalStdIO
  )
where

import           Data.List.NonEmpty  (NonEmpty (..))
import           Options.Applicative

import           P4P.RT
import           P4P.RT.Options

import           P4P.Sim.Extension
import           P4P.Sim.Internal
import           P4P.Sim.IO
import           P4P.Sim.Options
import           P4P.Sim.Types
