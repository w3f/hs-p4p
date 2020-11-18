{-# LANGUAGE TypeApplications #-}

import           Control.Applicative (liftA2)
import           System.Environment  (getArgs)
import           System.Exit         (ExitCode (..), exitWith)

import           P4P.RT
import           P4P.RT.EchoProcess
import           P4P.RT.Node


runEcho :: (SockEndpoint, RTOptions RTLogging) -> IO ExitCode
runEcho (addr, opt) = do
  let mkState = uncurry (flip EchoState) <$> initializeTickAddrs opt addr
      logging = defaultRTLogging opt
      mkLoIO  = udpRTLoIO @EchoState
      mkHiIO _ _ = do
        (stdio, close) <- do
          optionTerminalStdIO opt "p4p" ".echo_history" "p4p-echo> "
        pure (defaultRTHiIO @EchoState readEchoHiI showEchoHiO stdio, close)
  runProcIO' @EchoState opt mkState logging mkLoIO mkHiIO >>= handleRTResult

echoParseOptions :: [String] -> IO (SockEndpoint, RTOptions RTLogging)
echoParseOptions = parseArgsIO'
  "echo - an example p4p node"
  (defaultDescription
    "Example p4p node"
    (  "$addr :~ (<Rwd|Fwd>, $string) where $addr is a socket endpoint (same "
    <> "as --recv-addr), e.g. \"localhost:13337\", (Fwd, \"Hello, world!\"))"
    )
  )
  (liftA2 (,) (parseRecvAddr 13337) (rtOptions rtLogOptions))

main :: IO ()
main = getArgs >>= echoParseOptions >>= runEcho >>= exitWith
