
module Test.Golden where

import Test.Tasty (TestTree, testGroup)


goldenTests :: TestTree
goldenTests = testGroup "golden tests" []

-- checkFrontEndApiType :: ExpectedFrontEndApi :~: FrontEndApi
-- checkFrontEndApiType = Refl

createdCorrectlyTest :: TestTree
createdCorrectlyTest = undefined
  -- testCase "created correctly" $ checkFrontEndApiType @?= Refl
