{-# LANGUAGE TemplateHaskell #-}

module P4P.Proc.Lens where

-- external
import           Control.Lens.TH   (makePrisms)
import           Data.Schedule     (Tick)

-- internal
import           P4P.Proc.Internal


makePrisms ''RuntimeI

makePrisms ''RuntimeO

makePrisms ''GMsg

_RuntimeI_RTTick
  :: Applicative f
  => (Tick -> f (Tick, task))
  -> RuntimeI ()
  -> f (RuntimeI task)
_RuntimeI_RTTick f (RTTick tick _) = uncurry RTTick <$> f tick

-- | Convience lens composition for use with
-- 'Control.Monad.Trans.Schedule.State.tickTask'.
_MsgI_RTTick
  :: Applicative f
  => (Tick -> f (Tick, ti))
  -> GMsg (RuntimeI ()) u p
  -> f (GMsg (RuntimeI ti) u p)
_MsgI_RTTick = _MsgRT . _RuntimeI_RTTick
