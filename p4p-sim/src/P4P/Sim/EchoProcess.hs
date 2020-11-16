{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module P4P.Sim.EchoProcess where

-- external
import qualified Data.Map.Strict as M

import           Codec.Serialise (Serialise (..))
import           Data.Binary     (Binary (..))
import           Data.Word       (Word16)
import           GHC.Generics    (Generic)
import           P4P.Proc


type EAddr = Word16
type EchoMsg = String

data EchoState = EState
  { addrs :: ![EAddr]
  , count :: !Word16
  }
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

type EchoUserI = String
type EchoUserO = String

instance UProtocol EchoState where
  type Addr EchoState = EAddr
  type Msg EchoState = EchoMsg

instance ProcIface EchoState where
  type LoI EchoState = UPMsgI EchoState
  type LoO EchoState = UPMsgO EchoState
  type HiI EchoState = EchoUserI
  type HiO EchoState = EchoUserO

instance Proc EchoState where
  react i s = case i of
    MsgEnv _ ->
      -- automatically send a message every 1000 ticks
      let k   = count s
          a   = head $ addrs s
          msg = [ MsgLo (UData (succ a) "echo") | k `mod` 1000 == 0 ]
      in  (msg, s { count = if k == maxBound then 0 else succ k })
    MsgHi u              -> ([MsgHi u], s) -- echo reply back
    MsgLo (UData _ _   ) -> ([], s)
    MsgLo (UOwnAddr obs) -> if null obs
      then ([MsgLo (UOwnAddr (M.fromList $ (, ObsPositive 0) <$> addrs s))], s)
      else error "not implemented"
