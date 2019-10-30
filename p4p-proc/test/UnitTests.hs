import           Test.Tasty

import           P4P.Proc.UnitTests (tests)

main :: IO ()
main = do
  defaultMain $ testGroup "P4P.Proc.*" [P4P.Proc.UnitTests.tests]
