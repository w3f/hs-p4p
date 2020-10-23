{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- external
import           Codec.Serialise               (Serialise (..))
import           P4P.Proc                      (PAddr)

-- external, IO
import           System.Environment            (getArgs)
import           System.Exit                   (ExitCode (..), exitWith)

-- internal
import           P4P.Sim
import           P4P.Sim.Experiments.Extension
import           P4P.Sim.Experiments.Protocol


type SimC ps xs
  = ( Ord (PAddr ps)
    , SimReRe Serialise ps xs
    , SimReRe Show ps xs
    , SimReRe Read ps xs
    )

withSimProtoExt
  :: SimConvOptions (SimProto, SimExt)
  -> (forall ps xs . SimC ps xs => SProt ps -> SExt xs -> IO a)
  -> IO a
withSimProtoExt opt f = case proto of
  ProtoEcho -> f SEcho XNone
  ProtoKad  -> case ext of
    ExtNone -> f SKad XNone
    ExtKad  -> f SKad XKad
 where
  SimConvOptions {..} = opt
  (proto, ext)        = simConvXOpts

runMain :: SimConvOptions (SimProto, SimExt) -> IO ExitCode
runMain opt = withSimProtoExt opt $ \(p :: SProt ps) (x :: SExt xs) -> do
  convertSimData @ps @xs opt
  pure ExitSuccess

simConvParseOptions :: Parser xo -> [String] -> IO (SimConvOptions xo)
simConvParseOptions xopts = parseArgsIO'
  "sim-conv - a converter for p4p-sim data structures"
  "Read and write p4p-sim {input, output, state} in various formats."
  (simConvOptions xopts)

main :: IO ()
main =
  getArgs
    >>= simConvParseOptions (liftA2 (,) protoOptions extOptions)
    >>= runMain
    >>= exitWith
