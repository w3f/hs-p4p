{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module P4P.Sim.Options where

-- external
import           Control.Lens.TH.Extra (makeLenses_)
import           Control.Op
import           GHC.Generics          (Generic)
import           Options.Applicative
import           P4P.RT.Options        hiding (RTLogging (..))
import           Text.Read             (readEither)

-- internal
import           P4P.Sim.Numeric
import           P4P.Sim.Types


{- | Convert a 'ProcIOAction' representing 'initMode' into a delayed-init mode.

If we're all of:

  * not reading input state
  * reading empty input messages
  * not writing input messages, output messages, or output state

then the converted action will instead write the output-state to the
previously-specified input-state path, if any. This is useful for child
simulations that might want to implement additional startup behaviour, such as
constructing a network.

The 'Bool' result represents whether the conversion was performed or not.
-}
delayedInitMode :: ProcIOAction FilePath -> (Bool, ProcIOAction FilePath)
delayedInitMode = \case
  ProcIOAction { procIState = ProcIAction Nothing p, procIMsg = ProcIAction (Just e) Nothing, procOMsg = ProcOAction Nothing omc, procOState = ProcOAction Nothing osc }
    | e == systemEmptyFile
    -> ( True
       , ProcIOAction { procIState = ProcIAction Nothing Nothing
                      , procIMsg   = ProcIAction Nothing Nothing
                      , procOMsg   = ProcOAction Nothing omc
                      , procOState = ProcOAction p osc
                      }
       )
  a -> (False, a)

data SimLogging =
    LogNone
    -- ^ Log nothing
  | LogAux
    -- ^ Log aux messages, from both the simulation framework and its processes.
  | LogAuxProc
    -- ^ Also log messages between processes.
  | LogAuxProcHi
    -- ^ Also log user messages.
  | LogAuxProcHiEnvO
    -- ^ Also log EnvO, i.e. output timing signals.
  | LogAuxProcHiEnvIO
    -- ^ Log everything including input ticks - warning very spammy!
 deriving (Eq, Ord, Show, Read, Generic, Bounded, Enum)
makeLenses_ ''SimLogging

data SimOptions = SimOptions
  { simInitNodes    :: !Int
  , simInitLatency  :: !SimLatency
  -- :^ initial execution config, ignored during replay since it is read
  , simRTOptions    :: !(RTOptions SimLogging)
  , simDbgEmptySimX :: !Bool
  -- :^ debugging options
  }
  deriving (Eq, Show, Read, Generic)
makeLenses_ ''SimOptions

knownDistPosReader :: ReadM KnownDistPos
knownDistPosReader = eitherReader $ readEither >=> distPosToInternal

simLoggingOptions :: Parser SimLogging
simLoggingOptions =
  (  option auto
    <| long "logging"
    <> metavar "Logger"
    <> help ("Logging profile, " <> enumAllShow @SimLogging)
    <> completeWith (show <$> enumAll @SimLogging)
    <> value LogNone
    <> showDefault
    )
    <|> (   enumFromInt @SimLogging
        <$< length
        <$< many
        <|  flag' ()
        <|  short 'v'
        <>  help
              (  "Logging profile, occurence-counted flag. "
              <> enumAllShowInt @SimLogging
              )
        )

simOptions :: Parser SimOptions
simOptions =
  SimOptions
    <$> (  option auto
        <| long "num-nodes"
        <> short 'n'
        <> metavar "NUM"
        <> help
             "Initial number of nodes to launch. Ignored if reading from an existing input state, i.e. if --istate-r or --re* is given."
        <> value 1
        <> showDefault
        )
    <*> (   SLatAddrIndep
        <$< option knownDistPosReader
        <|  long "latency"
        <>  metavar "LAT"
        <>  help
              "Initial latency distribution, units in tick-delta. Ignored if reading from an existing input state, i.e. if --istate-r or --re* is given."
        <>  value (DistConstant 150)
        <>  showDefault
        )

    <*> rtOptions simLoggingOptions

    <*> (  switch
        <| long "dbg-empty-sim-x"
        <> help "For testing p4p-sim itself: use an empty extension"
        <> showDefault
        )


data SimXOptions xo = SimXOptions
  { simOpts  :: !SimOptions
  , simXOpts :: !xo
  }
  deriving (Eq, Show, Read, Generic)
makeLenses_ ''SimXOptions

simXOptions :: Parser xo -> Parser (SimXOptions xo)
simXOptions xopts = SimXOptions <$> simOptions <*> xopts
