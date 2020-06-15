{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- external
import           Control.Op
import           Data.Dependent.Sum        (DSum (..))
import           P4P.Proc                  (Proc, Protocol (..))

-- external, kademlia
import           P4P.Protocol.DHT.Kademlia (KState, defaultParams,
                                            newRandomState)

-- external, IO
import           System.Environment        (getArgs)
import           System.Exit               (ExitCode (..), exitWith)

-- internal
import           P4P.Sim
import           P4P.Sim.EchoProcess       (EchoState (..))
import           P4P.Sim.Options           (simParseOptions)
import           P4P.Sim.Util              (ChaChaDRGInsecure, PMut',
                                            getEntropy)
import           P4P.Sim.Util.IO           (bracketHEF, optionTerminalStdIO)


data SimProto = ProtoEcho | ProtoKad
 deriving (Eq, Ord, Show, Read, Bounded, Enum)

protoOptions :: Parser SimProto
protoOptions =
  option auto
    <| long "protocol"
    <> short 'p'
    <> metavar "Proto"
    <> help ("Protocol to simulate, " <> showOptions @SimProto)
    <> completeWith (show <$> allOptions @SimProto)
    <> value ProtoEcho
    <> showDefault

data SProt ps where
  SEcho :: SProt EchoState
  SKad :: SProt (KState ChaChaDRGInsecure)

withSProt
  :: SProt ps
  -> ((Read (UserI ps), Show (UserO ps), Read ps, Show ps) => a)
  -> a
withSProt prot a = case prot of
  SEcho -> a
  SKad  -> a

type SimC ps = (SimProcess (PMut' ps), SimLog ps (), SimReRe ps (), Proc ps)

withSimProto
  :: SimXOptions SimProto
  -> (forall ps . SimC ps => SProt ps -> (Pid -> IO ps) -> IO a)
  -> IO a
withSimProto opt f = case simXOpts of
  ProtoEcho -> f SEcho $ \p -> pure (EState [p] 0)
  ProtoKad  -> f SKad $ \p -> do
    let params = defaultParams $ fromIntegral $ 1000 `div` simMsTick simOpts
        addr   = "addr:" <> show p
    newRandomState @ChaChaDRGInsecure getEntropy [addr] params
  where SimXOptions {..} = opt

-- run via stdin/stdout
runStd :: SimXOptions SimProto -> IO ExitCode
runStd opt = withSimProto opt $ \(p :: SProt ps) mkPS -> withSProt p $ do
  let prompt  = "p4p " <> drop 5 (show simXOpts) <> "> "
  let mkStdIO = optionTerminalStdIO simOpts "p4p" ".sim_history" prompt
  bracketHEF mkStdIO $ \stdio -> do
    let simUserIO = defaultSimUserIO @ps @() stdio
    runSimIO @(PMut' ps) simOpts mkPS simUserIO >>= handleSimResult
  where SimXOptions {..} = opt

newtype UserSimAsync' ps = UserSimAsync' (UserSimAsync ps ())

-- run via tb-queues, can be loaded from GHCI
runTB :: SimXOptions SimProto -> IO (DSum SProt UserSimAsync')
runTB opt = withSimProto opt $ \(p :: SProt ps) mkPS -> withSProt p $ do
  let runSimIO' = runSimIO @(PMut' ps) simOpts mkPS
  handles <- newSimAsync @(PMut' ps) (Just print) runSimIO'
  pure $ p :=> UserSimAsync' handles
  where SimXOptions {..} = opt

main :: IO ()
main =
  getArgs
    >>= simParseOptions protoOptions
    -- "if True" avoids "unused" warnings for runTB
    >>= (if True then runStd else runTB >=> const (pure ExitSuccess))
    >>= exitWith
