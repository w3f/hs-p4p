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
import           Codec.Serialise              (Serialise (..))
import           Control.Op
import           Data.ByteString.Char8        (pack)
import           Data.Dependent.Sum           (DSum (..))
import           P4P.Proc                     (Proc, SockAddr (..),
                                               obsPositiveFromList)

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
    , SimLog Show ps
    , SimRe Serialise ps
    , SimUserRe ps ()
    , Proc ps
    )
class SimC' ps => SimC ps
instance SimC' ps => SimC ps

mkPState :: RTInitOptions init -> SProt ps -> Pid -> IO ps
mkPState opts prot p = do
  tick <- initializeTick opts
  case prot of
    SEcho -> pure $ EchoState (obsPositiveFromList 0 [SockAddrInet p 0]) tick
    SKad  -> do
      let addr   = pack $ "addr:" <> show p
          params = defaultParams $ fromIntegral $ 1000 `div` rtInitMsTick opts
      newRandomState @ChaChaDRGInsecure getEntropy tick [addr] params

-- run via stdin/stdout
runStd :: SimOptions SimProto -> IO ExitCode
runStd opt = withSimProto @SimC simXOpts $ \(p :: SProt ps) -> do
  let mkPS = mkPState simRTInitOptions p
  let mkSimUserIO = do
        let prompt = "p4p " <> drop 5 (show simXOpts) <> "> "
        (stdio, close) <- do
          optionTerminalStdIO simRTOptions "p4p" ".sim_history" prompt
        pure (defaultSimUserIO @ps @() stdio, close)
  runSimIO @(PMut' ps) opt mkPS mkSimUserIO >>= handleRTResult
  where SimOptions {..} = opt

newtype UserSimAsync' ps = UserSimAsync' (RTAsync (SimFullState ps ()))

-- run via tb-queues, can be loaded from GHCI
runTB :: SimOptions SimProto -> IO (DSum SProt UserSimAsync')
runTB opt = withSimProto @SimC simXOpts $ \(p :: SProt ps) -> do
  let mkPS = mkPState simRTInitOptions p
  let runSimIO' simUserIO =
        runSimIO @(PMut' ps) opt mkPS (pure (simUserIO, pure ()))
  handles <- newRTAsync @(SimFullState ps ()) (Just print) runSimIO'
  pure $ p :=> UserSimAsync' handles
  where SimOptions {..} = opt

simParseOptions :: Parser xo -> [String] -> IO (SimOptions xo)
simParseOptions xopts = parseArgsIO'
  "sim - a simulator for p4p protocols"
  (defaultDescription
    "Simulate a p4p protocol"
    (  "$pid :~ $command where $pid is a 16-bit process id and $command is a "
    <> "Haskell Show/Read expression of the protocol high-interface message "
    <> "type, e.g. for our example Echo protocol this could be "
    <> "(SockAddrInet 3 0, (Fwd, \"Hello, World!\"))"
    )
  )
  (simOptions xopts)

main :: IO ()
main =
  getArgs
    >>= simParseOptions protoOptions
    -- "if True" avoids "unused" warnings for runTB
    >>= (if True then runStd else runTB >=> const (pure ExitSuccess))
    >>= exitWith
