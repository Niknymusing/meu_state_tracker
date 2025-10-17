module MEU.Transforms.RefinementSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import MEU.Core.Types
import MEU.Transforms.Refinement

spec :: Spec
spec = describe "MEU Transforms Refinement" $ do
  describe "Placeholder Tests" $ do
    it "should always pass as placeholder" $ do
      True `shouldBe` True

    it "should test basic refinement creation" $ do
      let transform = createRefinementTransform "test" []
      transformId transform `shouldBe` "test"