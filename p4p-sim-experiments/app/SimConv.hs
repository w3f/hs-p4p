{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- external
import           Codec.Serialise               (Serialise (..))
import           P4P.Proc                      (Addr)

-- external, IO
import           System.Environment            (getArgs)
import           System.Exit                   (ExitCode (..), exitWith)

-- internal
import           P4P.Sim
import           P4P.Sim.Experiments.Extension
import           P4P.Sim.Experiments.Protocol


type SimPC' ps
  = (Ord (Addr ps), SimRe Serialise ps, SimRe Show ps, SimRe Read ps)
class SimPC' ps => SimPC ps
instance SimPC' ps => SimPC ps

type SimXC' xs = (SimXRe Serialise xs, SimXRe Show xs, SimXRe Read xs)
class SimXC' xs => SimXC xs
instance SimXC' xs => SimXC xs

runMain :: ConvOptions (SimProto, SimExt) -> IO ExitCode
runMain opt = withSimProto @SimPC proto $ \(p :: SProt ps) -> do
  withSimExt @SimXC ext $ \(x :: SExt xs) -> do
    convertProcData @(SimFullState ps xs) opt
    pure ExitSuccess
 where
  ConvOptions {..} = opt
  (proto, ext)     = convXOpts

simConvParseOptions :: Parser xo -> [String] -> IO (ConvOptions xo)
simConvParseOptions xopts = parseArgsIO'
  "sim-conv - a converter for p4p-sim data structures"
  "Read and write p4p-sim {input, output, state} in various formats."
  (convOptions xopts)

main :: IO ()
main =
  getArgs
    >>= simConvParseOptions (liftA2 (,) protoOptions extOptions)
    >>= runMain
    >>= exitWith
