{-| P4P simulations. -}

module P4P.Sim
  ( KV(..)
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
  -- external
  , StateT(..)
  , evalStateT
  )
where

import           Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import           Data.List.NonEmpty               (NonEmpty (..))
import           P4P.Sim.Internal
import           P4P.Sim.Types
