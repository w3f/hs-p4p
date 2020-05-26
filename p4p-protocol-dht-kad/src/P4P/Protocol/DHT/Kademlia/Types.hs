-- | Common internal types.
module P4P.Protocol.DHT.Kademlia.Types where

-- external
import qualified Data.Map.Strict                   as M
import qualified Data.Sequence.Extra               as S
import qualified Data.Set                          as Set

import           Data.Foldable                     (toList)
import           Data.Map.Strict                   (Map)
import           Data.Set                          (Set)

-- internal
import           P4P.Protocol.DHT.Kademlia.Message


{- | Internal node info, without (explicit) size limit.

TODO(addr): we'll want to add metadata to NodeAddr in future, to help us
choose between them. This will mirror the public NodeInfo type.
-}
type NodeInfos' = Map NodeId (Set NodeAddr)

toNodeInfos' :: NodeInfos -> NodeInfos'
toNodeInfos' ni =
  M.fromList $ (\(NodeInfo i a) -> (i, Set.fromList (toList a))) <$> toList ni

toNodeInfos :: NodeInfos' -> NodeInfos
toNodeInfos ni =
  S.bFromList $ (\(i, a) -> NodeInfo i (S.bFromList (toList a))) <$> M.toList ni

niUnion :: NodeInfos' -> NodeInfos' -> NodeInfos'
niUnion = M.unionWith Set.union
