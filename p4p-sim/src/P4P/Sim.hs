{-| P4P simulations. -}

module P4P.Sim
  ( KV(..)
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
  -- external
  , Map
  , fromSet
  , StateT(..)
  , evalStateT
  )
where

import           Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import           Data.List.NonEmpty               (NonEmpty (..))
import           Data.Map.Strict                  (Map, fromSet)
import           P4P.Sim.Internal
import           P4P.Sim.Types
