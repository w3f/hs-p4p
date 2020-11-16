{-| P4P runtime library.

This package implements record-and-replay behaviour for "P4P.Proc".

TODO: UDP server.
-}
module P4P.RT
  ( module P4P.RT.Options
  , RTError(..)
  , RTHiIO
  , runProcIO
  , handleRTResult
  , convertProcData
  , XTHiM
  , XTHiIO
  , RTAsync(..)
  , newRTAsync
  -- client
  , StdIO
  , bracketHEF
  , optionTerminalStdIO
  )
where

import           P4P.RT.Client
import           P4P.RT.Internal
import           P4P.RT.Options
