{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}

module P4P.Sim.Experiments.Extension where

-- external
import           Control.Op

-- external, p4p
import           P4P.Sim

-- internal
import           P4P.Sim.Experiments.Extension.Kademlia (KSimState)


data SimExt = ExtNone | ExtKad
 deriving (Eq, Ord, Show, Read, Bounded, Enum)

extOptions :: Parser SimExt
extOptions =
  option auto
    <| long "extension"
    <> short 'x'
    <> metavar "Ext"
    <> help ("Extension to use, " <> showOptions @SimExt)
    <> completeWith (show <$> allOptions @SimExt)
    <> value ExtNone
    <> showDefault

data SExt xs where
  XNone :: SExt ()
  XKad :: SExt KSimState

withSimExt
  :: (c (), c KSimState) => SimExt -> (forall xs . c xs => SExt xs -> a) -> a
withSimExt ext f = case ext of
  ExtNone -> f XNone
  ExtKad  -> f XKad
