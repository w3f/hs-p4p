{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- external
import           Control.Lens                           ((%%~), (&))
import           Crypto.Random.Extra                    (initializeFrom)

-- external, impure
import           System.Environment                     (getArgs)
import           System.Exit                            (exitWith)

-- external, p4p
import           P4P.Protocol.DHT.Kademlia
import           P4P.Sim
import           P4P.Sim.Experiments.Extension.Kademlia
import           P4P.Sim.Options                        (delayedInitMode)
import           P4P.Sim.Util                           (ChaChaDRGInsecure,
                                                         PMut', getEntropy)


type KProc = PMut' KS

mkPState :: RTInitOptions init -> KParams -> Pid -> IO KS
mkPState opt params p = do
  tick <- initializeTick opt
  newRandomState @ChaChaDRGInsecure getEntropy tick [mkAddr p] params

main :: IO ()
main = do
  let parser = mkParser "sim-kad" "kademlia test" (simOptions kadOptions)
  simOpts <- parseArgsIO parser =<< getArgs
  let SimOptions {..} = simOpts
  let RTOptions {..}  = simRTOptions

  -- auto-join if we're
  --   - not reading input state
  let autoJoin = case procIActRead (procIState rtProcIOAction) of
        Nothing -> True
        Just _  -> False
  -- auto-quit if we're perform an init-mode action. in this case, we will
  -- auto-join (i.e. send some messages), then write the output state to the
  -- specified input-state-file (if any), then quit
  let (autoQuit, simOpts') =
        simOpts & _simRTOptions . _rtProcIOAction %%~ delayedInitMode

  let mkSimUserIO = do
        (stdio, close) <- do
          optionTerminalStdIO simRTOptions "p4p" ".sim-kad_history" "p4p Kad> "
        let joinStarted = \case
              KSimJoinStarted -> True
              _               -> False
        simUserIO <-
          hookAutoJoinQuit @_ @KSimState autoJoin
                                         autoQuit
                                         KSimJoinAll
                                         joinStarted
            $ defaultSimUserIO @KS @KSimState stdio
        pure (simUserIO, close)

      mkXState = do
        drg <- initializeFrom getEntropy
        pure $ KSimState drg Nothing

  let initOpts = simRTInitOptions
  let params = simXOpts $ fromIntegral $ 1000 `div` rtInitMsTick initOpts
  grunSimIO @KS @KSimState (runSimXS @KProc @KSimState)
                           simOpts'
                           mkXState
                           (mkPState initOpts params)
                           mkSimUserIO
    >>= handleRTResult
    >>= exitWith
