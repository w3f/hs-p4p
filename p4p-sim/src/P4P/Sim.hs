{-| P4P simulations. -}

module P4P.Sim
  (
  -- * Main definitions
    Pid
  , SimHiI'(..)
  , SimHiI
  , SimHiO'(..)
  , SimHiO
  , SimAuxO'(..)
  , SimAuxO
  , SimState'(..)
  , SimState
  , SimFullState
  , NonEmpty(..)
  , newSimState
  , SimT
  , SimProcess
  , runSim
  -- * Extensions
  , SimXProcIface(..)
  , runSimX
  , runSimXS
  -- * Options
  , SimOptions(..)
  , _simRTInitOptions
  , _simRTOptions
  , simOptions
  , module Options.Applicative
  , module P4P.RT.Options
  -- * IO and utilities
  , SimLog
  , SimRe
  , SimXRe
  , SimUserRe
  , defaultSimUserIO
  , grunSimIO
  , runSimIO
  , hookAutoJoinQuit
  -- * RT generic utils
  , handleRTResult
  , convertProcData
  , RTAsync(..)
  , newRTAsync
  , optionTerminalStdIO
  , initializeTick
  , KV(..)
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
