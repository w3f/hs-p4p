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
import           P4P.Sim.Options                        (SimIAction (..),
                                                         SimIOAction (..),
                                                         delayedInitMode,
                                                         _simIOAction)
import           P4P.Sim.Util                           (ChaChaDRGInsecure,
                                                         PMut', getEntropy)
import           P4P.Sim.Util.IO                        (bracketHEF,
                                                         hookAutoJoinQuit,
                                                         optionTerminalStdIO)


type KProc = PMut' KS

mkPState :: KParams -> Pid -> IO KS
mkPState params p =
  let addr = mkAddr p
  in  newRandomState @ChaChaDRGInsecure getEntropy [addr] params

main :: IO ()
main = do
  let parser = mkParser "sim-kad" "kademlia test" (simXOptions kadOptions)
  SimXOptions {..} <- parseArgsIO parser =<< getArgs

  -- auto-join if we're
  --   - not reading input state
  let autoJoin = case simIActRead (simIState (simIOAction simOpts)) of
        Nothing -> True
        Just _  -> False
  -- auto-quit if we're perform an init-mode action. in this case, we will
  -- auto-join (i.e. send some messages), then write the output state to the
  -- specified input-state-file (if any), then quit
  let (autoQuit, simOpts') = simOpts & _simIOAction %%~ delayedInitMode

  let mkStdIO =
        optionTerminalStdIO simOpts "p4p" ".sim-kad_history" "p4p Kad> "
  bracketHEF mkStdIO $ \stdio -> do
    let joinStarted = \case
          KSimJoinStarted -> True
          _               -> False
    simUserIO <-
      hookAutoJoinQuit @_ @KSimState autoJoin autoQuit KSimJoinAll joinStarted
        $ defaultSimUserIO @KS @KSimState stdio

    drg <- initializeFrom getEntropy
    let initXState = KSimState drg Nothing
    let params = simXOpts $ fromIntegral $ 1000 `div` simMsTick simOpts
    grunSimIO @KProc @KSimState (runSimXS @KProc @KSimState)
                                simOpts'
                                initXState
                                (mkPState params)
                                simUserIO
      >>= handleSimResult
      >>= exitWith
