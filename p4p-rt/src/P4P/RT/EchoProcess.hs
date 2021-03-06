{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module P4P.RT.EchoProcess where

-- external
import           Codec.Serialise (Serialise (..))
import           Data.Binary     (Binary (..))
import           Data.Schedule   (HasNow (..))
import           GHC.Generics    (Generic)
import           P4P.Proc        hiding (Direction)
import           Text.Read       (readEither)

-- internal
import           P4P.RT.Client   (KV (..))
import           P4P.RT.Network


data Direction = Rwd | Fwd
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

type EchoMsg = (Direction, String)

data EchoState = EchoState
  { addrs :: !(Observations SockAddr)
  , count :: !Tick
  }
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

instance HasNow EchoState where
  getNow = count

instance Protocol EchoState where
  type XMsg EchoState = EchoMsg
  getAddrs = obsPositiveToSet . addrs

instance ProcIface EchoState where
  type LoI EchoState = UPMsgI EchoState
  type LoO EchoState = UPMsgO EchoState
  type HiI EchoState = (SockAddr, EchoMsg)
  type HiO EchoState = (SockAddr, EchoMsg)
  type AuxO EchoState = ProtocolCodecError

instance UProtocol EchoState where

instance Proc EchoState where
  react = withCodec cborCodec16 id $ \i s -> case i of
    MsgEnv t          -> ([], s { count = t })
    MsgHi  (dst, msg) -> ([MsgLo (UData dst (Val msg))], s) -- pass it on
    MsgLo (UData src (Mal _)) -> ([], s) -- ignore malformed messages
    MsgLo (UData src (Val msg@(dir, body))) -> case dir of
      Rwd -> ([MsgLo (UData src (Val (Fwd, body)))], s) -- echo reply back
      Fwd -> ([MsgHi (src, msg)], s) -- forward it to the user
    MsgLo (UOwnAddr obs) -> if null obs
      then ([MsgLo (UOwnAddr (addrs s))], s)
      else ([], s { addrs = updateTrustedObs (addrs s) obs })

readEchoHiI :: String -> IO (Either String (HiI EchoState))
readEchoHiI s = case readEither s of
  Left  e          -> pure (Left e)
  Right (sep :~ v) -> case readSockEndpoint 0 sep of
    Left  e  -> pure (Left e)
    Right ep -> resolveEndpoint ep >>= \case
      []     -> pure (Left $ "could not resolve hostname: " <> sep)
      h : tl -> pure (Right (fromNAddr h, v))

showEchoHiO :: HiO EchoState -> IO (Maybe String)
showEchoHiO (dst, msg) = pure $ Just $ show $ showSockEndpoint ep :~ msg
  where ep = EndpointByAddr (toNAddr dst)
