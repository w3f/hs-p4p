{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}

module P4P.Sim.Experiments.Protocol where

-- external
import           Control.Op

-- external, p4p
import           P4P.Protocol.DHT.Kademlia (KState)
import           P4P.RT.EchoProcess        (EchoState (..))
import           P4P.Sim
import           P4P.Sim.Util              (ChaChaDRGInsecure)


data SimProto = ProtoEcho | ProtoKad
 deriving (Eq, Ord, Show, Read, Bounded, Enum)

protoOptions :: Parser SimProto
protoOptions =
  option auto
    <| long "protocol"
    <> short 'p'
    <> metavar "Proto"
    <> help ("Protocol to use, " <> showOptions @SimProto)
    <> completeWith (show <$> allOptions @SimProto)
    <> value ProtoEcho
    <> showDefault

data SProt ps where
  SEcho :: SProt EchoState
  SKad :: SProt (KState ChaChaDRGInsecure)

withSimProto
  :: (c EchoState, c (KState ChaChaDRGInsecure))
  => SimProto
  -> (forall ps . c ps => SProt ps -> a)
  -> a
withSimProto proto f = case proto of
  ProtoEcho -> f SEcho
  ProtoKad  -> f SKad
