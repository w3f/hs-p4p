{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- external
import           Codec.Serialise              (Serialise (..))
import           Control.Op
import           Data.ByteString.Char8        (pack)
import           Data.Dependent.Sum           (DSum (..))
import           P4P.Proc                     (Proc, SockAddr (..))

-- external, protocol
import           P4P.Protocol.DHT.Kademlia    (defaultParams, newRandomState)
import           P4P.RT.EchoProcess           (EchoState (..))

-- external, IO
import           System.Environment           (getArgs)
import           System.Exit                  (ExitCode (..), exitWith)

-- internal
import           P4P.Sim
import           P4P.Sim.Experiments.Protocol
import           P4P.Sim.Util                 (ChaChaDRGInsecure, PMut',
                                               getEntropy)


type SimC' ps
  = ( SimProcess (PMut' ps)
    , SimLog ps ()
    , SimUserRe ps ()
    , SimReRe Serialise ps ()
    , Proc ps
    )
class SimC' ps => SimC ps
instance SimC' ps => SimC ps

mkPState :: SimOptions -> SProt ps -> Pid -> IO ps
mkPState simOpts = \case
  SEcho -> \p -> pure (EState [SockAddrInet p 0] 0)
  SKad  -> \p -> do
    let params = defaultParams $ fromIntegral $ 1000 `div` rtMsTick procOpts
        addr   = pack $ "addr:" <> show p
    newRandomState @ChaChaDRGInsecure getEntropy [addr] params
  where procOpts = simRTOptions simOpts

-- run via stdin/stdout
runStd :: SimXOptions SimProto -> IO ExitCode
runStd opt = withSimProto @SimC simXOpts $ \(p :: SProt ps) -> do
  let mkPS    = mkPState simOpts p
  let prompt  = "p4p " <> drop 5 (show simXOpts) <> "> "
  let mkStdIO = optionTerminalStdIO simRTOptions "p4p" ".sim_history" prompt
  bracketHEF mkStdIO $ \(iact, stdio) -> do
    let simUserIO = defaultSimUserIO @ps @() stdio
    runSimIO @(PMut' ps) simOpts mkPS iact simUserIO >>= handleRTResult
 where
  SimXOptions {..} = opt
  SimOptions {..}  = simOpts

newtype UserSimAsync' ps = UserSimAsync' (RTAsync (SimFullState ps ()))

-- run via tb-queues, can be loaded from GHCI
runTB :: SimXOptions SimProto -> IO (DSum SProt UserSimAsync')
runTB opt = withSimProto @SimC simXOpts $ \(p :: SProt ps) -> do
  let mkPS      = mkPState simOpts p
  let runSimIO' = runSimIO @(PMut' ps) simOpts mkPS False
  handles <- newRTAsync @(SimFullState ps ()) (Just print) runSimIO'
  pure $ p :=> UserSimAsync' handles
  where SimXOptions {..} = opt

simParseOptions :: Parser xo -> [String] -> IO (SimXOptions xo)
simParseOptions xopts = parseArgsIO'
  "sim - a simulator for p4p protocols"
  (  "Simulate a p4p protocol. Commands are given on stdin and replies "
  <> "are given on stdout. The syntax is $pid :~ $command where $pid "
  <> "and $command are Haskell Show/Read instance expressions, e.g. 0 :~ "
  <> "\"Hello, world!\". Give -v for more detailed output."
  )
  (simXOptions xopts)

main :: IO ()
main =
  getArgs
    >>= simParseOptions protoOptions
    -- "if True" avoids "unused" warnings for runTB
    >>= (if True then runStd else runTB >=> const (pure ExitSuccess))
    >>= exitWith
