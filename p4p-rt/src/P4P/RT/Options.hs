{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module P4P.RT.Options where

-- external
import           Control.Lens.TH.Extra          (makeLenses_)
import           Control.Op
import           Foreign.C.Types                (CInt)
import           GHC.Generics                   (Generic)
import           Options.Applicative
import           Options.Applicative.Help.Chunk


showOptions :: forall a . (Show a, Enum a, Bounded a) => String
showOptions = "one of: " <> show (allOptions :: [a])

allOptions :: forall a . (Enum a, Bounded a) => [a]
allOptions = enumFrom minBound

helps :: [String] -> Mod f a
helps strs = helpDoc $ Just $ extractChunk $ vsepChunks $ fmap paragraph strs

data ProcIAction p = ProcIAction
  { procIActRead  :: !(Maybe p)
    -- ^ Take input by reading from the given path.
    --
    -- The path must exist already. If 'Nothing' is given then the input will
    -- be read in some default way, dependent on the context.
  , procIActWrite :: !(Maybe p)
    -- ^ For all input, write it into the given path.
    --
    -- The path must not exist.
  }
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)
makeLenses_ ''ProcIAction

data ProcOAction p = ProcOAction
  { procOActWrite   :: !(Maybe p)
    -- ^ For all output, write it into the given path.
    --
    -- The path must not exist.
  , procOActCompare :: !(Maybe p)
    -- ^ For all output, compare with the pre-recorded path.
    --
    -- The path must exist already.
  }
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)
makeLenses_ ''ProcOAction

data ProcIOAction p = ProcIOAction
  { procIState :: !(ProcIAction p)
    -- ^ How we should take input state.
    --
    -- If no source is given, we use a default "null initial" state.
  , procIMsg   :: !(ProcIAction p)
    -- ^ How we should take input messages.
    --
    -- If no source is given, we use standard input.
  , procOMsg   :: !(ProcOAction p)
    -- ^ What to do with output messages.
  , procOState :: !(ProcOAction p)
  }
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)
makeLenses_ ''ProcIOAction

actionOptions :: Parser (ProcIOAction FilePath)
actionOptions =
  ProcIOAction
    <$> (   ProcIAction
        <$> optional
              (  strOption
              <| long "istate-r"
              <> metavar "FILE"
              <> help
                   "Read input state from this file. If given, the file must exist. If this not given, then a default empty initial state will be used, specific to the protocol chosen."
              )
        <*> optional
              (strOption <| long "istate-w" <> metavar "FILE" <> help
                "Write input state to this file, which must not exist."
              )
        )
    <*> (   ProcIAction
        <$> optional
              (  strOption
              <| long "imsg-r"
              <> metavar "FILE"
              <> help
                   "Read input messages from this file. If given, the file must exist. If not given, then messages will be taken from stdin. "
              )
        <*> optional
              (strOption <| long "imsg-w" <> metavar "FILE" <> help
                "Write input messages to this file, which must not exist."
              )
        )
    <*> (   ProcOAction
        <$> optional
              (strOption <| long "omsg-w" <> metavar "FILE" <> help
                "Write output messages to this file, which must not exist."
              )
        <*> optional
              (strOption <| long "omsg-c" <> metavar "FILE" <> help
                "Compare output messages with this file, which must exist."
              )
        )
    <*> (   ProcOAction
        <$> optional
              (strOption <| long "ostate-w" <> metavar "FILE" <> help
                "Write output state to this file, which must not exist."
              )
        <*> optional
              (strOption <| long "ostate-c" <> metavar "FILE" <> help
                "Compare output state with this file, which must exist."
              )
        )

filespecReader :: ReadM (FilePath, FilePath, FilePath)
filespecReader = eitherReader $ \s -> case span (/= ':') (reverse s) of
  (rx@(_ : _), ':' : 'i' : '.' : residue) -> case span (/= ':') residue of
    (ri@(_ : _), ':' : 's' : '.' : rs@(_ : _)) ->
      Right (reverse rs, reverse ri, reverse rx)
    _ -> Left "FILESPEC syntax error"
  _ -> Left "FILESPEC syntax error"

succString :: String -> String
succString x =
  let r      = reverse x
      (d, s) = span (`elem` "0123456789") r
      l      = length d
  in  if l == 0
        then x <> "+"
        else
          let n  = read (reverse d) :: Integer
              d' = show (succ n)
              l' = length d'
              p  = if l' < l then replicate (l - l') '0' else []
          in  reverse s <> p <> d'

-- TODO: cross-platform
systemEmptyFile :: FilePath
systemEmptyFile = "/dev/null"

initMode :: (FilePath, FilePath, FilePath) -> ProcIOAction FilePath
initMode (s, i, x) =
  let s' = s <> ".s"
      is = ProcIAction Nothing (Just s')
      im = ProcIAction (Just systemEmptyFile) Nothing
      om = ProcOAction Nothing Nothing
      os = ProcOAction Nothing Nothing
  in  ProcIOAction is im om os

recordMode :: (FilePath, FilePath, FilePath) -> ProcIOAction FilePath
recordMode (s, i, x) =
  let s' = s <> ".s"
      i' = s' <> ":" <> i <> ".i"
      is = ProcIAction (Just s') Nothing
      im = ProcIAction Nothing (Just i')
      om = ProcOAction (Just $ i' <> ":" <> x <> ".o") Nothing
      os = ProcOAction (Just $ i' <> ":" <> x <> ".s") Nothing
  in  ProcIOAction is im om os

replayMode :: (FilePath, FilePath, FilePath) -> ProcIOAction FilePath
replayMode (s, i, x) =
  let s' = s <> ".s"
      i' = s' <> ":" <> i <> ".i"
      is = ProcIAction (Just s') Nothing
      im = ProcIAction (Just i') Nothing
      om = ProcOAction Nothing (Just $ i' <> ":" <> x <> ".o")
      os = ProcOAction Nothing (Just $ i' <> ":" <> x <> ".s")
  in  ProcIOAction is im om os

rereMode :: (FilePath, FilePath, FilePath) -> ProcIOAction FilePath
rereMode (s, i, x) =
  let s'  = s <> ".s"
      i'  = s' <> ":" <> i <> ".i"
      i'' = i' <> ":"
      y   = succString x
      is  = ProcIAction (Just s') Nothing
      im  = ProcIAction (Just i') Nothing
      om  = ProcOAction (Just $ i'' <> y <> ".o") (Just $ i'' <> x <> ".o")
      os  = ProcOAction (Just $ i'' <> y <> ".s") (Just $ i'' <> x <> ".s")
  in  ProcIOAction is im om os

-- TODO: ideally we'd group the option help text together but
-- https://github.com/pcapriotti/optparse-applicative/issues/270
allActionOptions :: Parser (ProcIOAction FilePath)
allActionOptions =
  actionOptions
    <|> (   initMode
        <$< option filespecReader
        <|  long "init"
        <>  metavar "FILESPEC"
        <>  helps
              [ "Init mode for a FILESPEC $S.s:$I.i:$X will:"
              , "1. write a default empty input state into $S.s"
              , "2. exit immediately"
              , "It is mutually exclusive with --re* and the --[io]{state,msg}-* options."
              ]
        )
    <|> (   recordMode
        <$< option filespecReader
        <|  long "record"
        <>  metavar "FILESPEC"
        <>  helps
              [ "Record mode for a FILESPEC $S.s:$I.i:$X will:"
              , "1. read input state from    $S.s"
              , "2. write input  messages to $S.s:$I.i, reading them from stdin"
              , "3. write output messages to $S.s:$I.i:$X.o"
              , "4. write output state    to $S.s:$I.i:$X.s"
              , "It is mutually exclusive with --init, --re* and the --[io]{state,msg}-* options."
              ]
        )
    <|> (   replayMode
        <$< option filespecReader
        <|  long "replay"
        <>  metavar "FILESPEC"
        <>  helps
              [ "Replay mode for a FILESPEC $S.s:$I.i:$X will:"
              , "1. read input state        from $S.s"
              , "2. read input messages     from $S.s:$I.i"
              , "3. compare output messages with $S.s:$I.i:$X.o"
              , "4. compare output state    with $S.s:$I.i:$X.s"
              , "It is mutually exclusive with --init, --re* and the --[io]{state,msg}-* options."
              ]
        )
    <|> (   rereMode
        <$< option filespecReader
        <|  long "rere"
        <>  metavar "FILESPEC"
        <>  helps
              [ "Rere (record-and-replay) mode for a FILESPEC $S.s:$I.i:$X will:"
              , "1. read input state    from $S.s"
              , "2. read input messages from $S.s:$I.i"
              , "3. write output messages to $S.s:$I.i:$((X + 1)).o and compare them with $S.s:$I.i:$X.o"
              , "4. write output state    to $S.s:$I.i:$((X + 1)).s and compare them with $S.s:$I.i:$X.s"
              , "It is mutually exclusive with --init, --re* and the --[io]{state,msg}-* options."
              ]
        )

data RTLogging =
    LogNone
    -- ^ Log nothing
  | LogLo
    -- ^ Log all MsgLo, i.e. network traffic
  | LogLoHi
    -- ^ Log all MsgLo and MsgHi, i.e. network traffic and user IO.
  | LogLoHiEnv
    -- ^ Log everything including ticks - warning very spammy!
 deriving (Eq, Ord, Show, Read, Generic, Bounded, Enum)
makeLenses_ ''RTLogging

rtLogOptions :: Parser RTLogging
rtLogOptions =
  (  option auto
    <| long "logging"
    <> metavar "Logger"
    <> help ("Logging profile, " <> showOptions @RTLogging)
    <> completeWith (show <$> allOptions @RTLogging)
    <> value LogNone
    <> showDefault
    )
    <|> (   loggerFromInt
        <$< length
        <$< many
        <|  flag' ()
        <|  short 'v'
        <>  help
              (  "Logging profile, occurence-counted flag. "
              <> loggerFromIntStrDesc
              )
        )
 where
  loggerFromInt :: Int -> RTLogging
  loggerFromInt i | i > 2     = LogLoHiEnv
                  | i == 2    = LogLoHi
                  | i == 1    = LogLo
                  | otherwise = LogNone

  loggerFromIntStrDesc :: String
  loggerFromIntStrDesc =
    "0 -> LogNone, 1 -> LogLo, 2 -> LogLoHi, 3+ -> LogLoHiEnv."

data RTOptions log = RTOptions
  { rtMsTick       :: !Integer
  -- :^ initial execution config, ignored during replay since it is read
  , rtProcIOAction :: !(ProcIOAction FilePath)
  -- :^ IO options, inc. record/replay
  , rtLogging      :: !log
  , rtLogOutput    :: !(Either CInt FilePath)
  , rtLogTimeFmt   :: !String
  }
  deriving (Eq, Show, Read, Generic)
makeLenses_ ''RTOptions

rtOptions :: Parser log -> Parser (RTOptions log)
rtOptions logOptions =
  RTOptions
    <$> (  option auto
        <| long "ms-per-tick"
        <> short 't'
        <> metavar "MS"
        <> help
             "Milliseconds in a tick. Ignored if reading from existing input messages, i.e. if --imsg-r or --replay or --rere is given."
        <> value 1
        <> showDefault
        )

    <*> allActionOptions

    <*> logOptions

    <*> (   (   Left
            <$< option auto
            <|  long "log-fd"
            <>  metavar "FD"
            <>  help "Log to a file descriptor."
            <>  value 2
            <>  showDefault
            )
        <|> (   Right
            <$< strOption
            <|  long "log-file"
            <>  short 'f'
            <>  metavar "FILE"
            <>  help "Log to a file."
            <>  action "file"
            )
        )
    <*> (  strOption
        <| long "log-time-fmt"
        <> metavar "FMT"
        <> help "Logging timestamp format-string."
        <> value "%Y-%m-%d %H:%M:%S.%3q %z"
        <> showDefault
        )

data ConvOptions xo = ConvOptions
  { convIFile :: !(Either CInt FilePath)
  , convOFile :: !(Either CInt FilePath)
  , convXOpts :: !xo
  }
  deriving (Eq, Show, Read, Generic)
makeLenses_ ''ConvOptions

convOptions :: Parser xo -> Parser (ConvOptions xo)
convOptions xopts =
  ConvOptions
    <$> (   (   Left
            <$< option auto
            <|  long "input-fd"
            <>  metavar "FD"
            <>  help "Read input from a file descriptor."
            <>  value 0
            <>  showDefault
            )
        <|> (   Right
            <$< strOption
            <|  long "input-file"
            <>  short 'i'
            <>  metavar "FILE"
            <>  help "Read input from a file."
            <>  action "file"
            )
        )
    <*> (   (   Left
            <$< option auto
            <|  long "output-fd"
            <>  metavar "FD"
            <>  help "Write output to a file descriptor."
            <>  value 1
            <>  showDefault
            )
        <|> (   Right
            <$< strOption
            <|  long "output-file"
            <>  short 'o'
            <>  metavar "FILE"
            <>  help "Write output to a file."
            <>  action "file"
            )
        )
    <*> xopts

defaultDescription :: String -> String -> String
defaultDescription synopsis syntax =
  synopsis
    <> ". Commands are given on stdin and replies are given on "
    <> "stdout. The syntax is "
    <> syntax
    <> ". Give -v for more detailed output."


mkParser :: String -> String -> Parser opt -> ParserInfo opt
mkParser summary desc parser = info
  (helper <*> parser)
  (fullDesc <> header summary <> progDesc desc <> failureCode 2)

parseArgsIO :: ParserInfo opt -> [String] -> IO opt
parseArgsIO parser args =
  execParserPure defaultPrefs parser args |> handleParseResult

parseArgsIO' :: String -> String -> Parser opt -> [String] -> IO opt
parseArgsIO' title desc parseOpt = parseArgsIO $ mkParser title desc parseOpt
