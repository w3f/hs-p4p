import           Test.Tasty

import           P4P.Protocol.DHT.Kademlia.UnitTests (tests)

main :: IO ()
main = do
  defaultMain $ testGroup "P4P.Protocol.DHT.Kademlia.*"
                          [P4P.Protocol.DHT.Kademlia.UnitTests.tests]
