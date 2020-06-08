{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module P4P.Sim.EchoProcess where

-- external
import           Data.Word    (Word16)
import           GHC.Generics (Generic)
import           P4P.Proc     (GMsg (..), Proc (..), ProtoMsg (..),
                               Protocol (..))


type EAddr = Word16
data EchoMsg = EMsg
  { src :: !EAddr
  , dst :: !EAddr
  }
  deriving (Eq, Ord, Show, Read, Generic)

data EchoState = EState
  { addrs :: ![EAddr]
  , count :: !Word16
  }
  deriving (Eq, Ord, Show, Read, Generic)

type EchoUserI = String
type EchoUserO = String

instance ProtoMsg EchoMsg where
  type Addr EchoMsg = EAddr
  getTarget = dst
  setSource src' m = m { src = src' }

instance Protocol EchoState where
  type PMsg EchoState = EchoMsg
  type UserI EchoState = EchoUserI
  type UserO EchoState = EchoUserO

instance Proc EchoState where
  getAddrs = addrs
  localNow _ = 0
  react i s = case i of
    MsgRT   _   -> ([], s)
    MsgUser u   -> ([MsgUser u], s) -- echo reply back
    MsgProc msg -> ([], s)
