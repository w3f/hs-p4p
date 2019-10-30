module P4P.Proc.UnitTests where

-- external
import           Test.Tasty       hiding (after)
import           Test.Tasty.HUnit


smoke :: IO ()
smoke = do
  pure ()

tests :: TestTree
tests = testGroup "P4P.Proc.UnitTests" [testCase "smoke" smoke]
