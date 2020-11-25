{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module P4P.RT.Internal.Serialise where

-- external
import qualified Codec.CBOR.Read                 as CBOR.Read
import qualified Data.ByteString.Lazy.Char8      as LBS
import qualified Data.Text.Lazy                  as T
import qualified Text.ParserCombinators.ReadP    as Read
import qualified Text.ParserCombinators.ReadPrec as Read

import           Codec.Serialise                 (Serialise (..),
                                                  deserialiseFullyOrFail,
                                                  serialise)
import           Control.Monad.Primitive         (PrimMonad (..))
import           Data.Bifunctor                  (Bifunctor (..))
import           Data.Binary                     (Binary)
import           Data.Either                     (partitionEithers)
import           Data.Primitive.MutVar           (MutVar (..), newMutVar,
                                                  readMutVar, writeMutVar)
import           GHC.Generics                    (Generic)
import           GHC.Stack                       (HasCallStack)
import           System.IO                       (Handle)
import           Text.Pretty.Simple              (StringOutputStyle (..),
                                                  defaultOutputOptionsNoColor,
                                                  outputOptionsStringStyle,
                                                  pShowOpt)
import           Text.Read                       (readPrec)


newtype Stream s = Stream (MutVar (PrimState IO) s)

newLBStream :: Handle -> IO (Stream LBS.ByteString)
newLBStream h = do
  bs <- LBS.hGetContents h
  mv <- newMutVar bs
  pure $ Stream mv

withStreamOrEOF
  :: (Monoid s, Eq s)
  => String
  -> Stream s
  -> (s -> Either String (s, a))
  -> IO (Maybe a)
withStreamOrEOF ctx (Stream mv) deser = do
  bs0 <- readMutVar mv
  if bs0 == mempty
    then pure Nothing
    else do
      let (unused, v) = case deser bs0 of
            Left  err -> error $ ctx <> ": " <> err
            Right a   -> a
      writeMutVar mv unused
      pure $ Just v

readContinue :: Read a => String -> Either String (String, a)
readContinue s = case Read.readPrec_to_S read' Read.minPrec s of
  [(x, unused)] -> Right (unused, x)
  []            -> Left "Prelude.read: no parse"
  _             -> Left "Prelude.read: ambiguous parse"
 where
  read' = do
    x <- readPrec
    Read.lift Read.skipSpaces
    return x

readOrEOF :: Read a => String -> Stream String -> IO (Maybe a)
readOrEOF ctx s = withStreamOrEOF ctx s readContinue

deserialiseContinue
  :: Serialise a => LBS.ByteString -> Either String (LBS.ByteString, a)
deserialiseContinue = first show . CBOR.Read.deserialiseFromBytes decode

deserialiseOrEOF
  :: Serialise a => String -> Stream LBS.ByteString -> IO (Maybe a)
deserialiseOrEOF ctx s = withStreamOrEOF ctx s deserialiseContinue

deserialiseNote :: (HasCallStack, Serialise a) => String -> LBS.ByteString -> a
deserialiseNote ctx bs = case deserialiseFullyOrFail bs of
  Left  err -> error $ ctx <> ": " <> show err
  Right a   -> a

data CodecWith f a where
  CodecWith :: (Monoid s, Eq s) => (s -> Either String (s, a)) -> (a -> s) -> f s -> CodecWith f a

type SomeDecode a = CodecWith ((->) LBS.ByteString) a
type SomeResidue a = CodecWith Maybe a

someDecodeStream
  :: Show k
  => LBS.ByteString
  -> [(k, SomeDecode a)]
  -> Either [String] ((k, a), SomeResidue a)
someDecodeStream bs0 decodes =
  let tryDecode = \case
        (k, CodecWith dec enc toS) -> case dec (toS bs0) of
          Left err -> Left $ show k <> ": " <> err
          Right (unused, v) ->
            let residue = if unused == mempty then Nothing else Just unused
            in  Right ((k, v), CodecWith dec enc residue)
      decoded           = fmap tryDecode decodes
      (errors, results) = partitionEithers decoded
  in  if null results then Left errors else Right $ head results

withDecodeStream
  :: Monad m => String -> (a -> m ()) -> a -> SomeResidue a -> m ()
withDecodeStream ctx act v (CodecWith dec enc res') = do
  act v
  case res' of
    Nothing  -> pure ()
    Just res -> case dec res of
      Left  err          -> error $ ctx <> ": " <> err
      Right (unused, v') -> do
        let res'' = if unused == mempty then Nothing else Just unused
        withDecodeStream ctx act v' $ CodecWith dec enc res''

showLn :: Show a => a -> String
showLn x = show x <> "\n"

pShowLn :: Show a => a -> String
pShowLn x = T.unpack (pShowOpt opt x) <> "\n"
 where
  opt = defaultOutputOptionsNoColor { outputOptionsStringStyle = Literal }

cborCodec :: Serialise a => SomeDecode a
cborCodec = CodecWith deserialiseContinue serialise id

readCodec :: (Show a, Read a) => SomeDecode a
readCodec = CodecWith readContinue showLn LBS.unpack

data CodecFormat = CodecCbor | CodecRead
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

allCodecs :: (Serialise a, Show a, Read a) => [(CodecFormat, SomeDecode a)]
allCodecs = [(CodecCbor, cborCodec), (CodecRead, readCodec)]
