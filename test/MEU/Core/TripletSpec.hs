module MEU.Core.TripletSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import MEU.Core.Types
import MEU.Core.Triplet

spec :: Spec
spec = describe "MEU Core Triplet" $ do
  describe "Placeholder Tests" $ do
    it "should always pass as placeholder" $ do
      True `shouldBe` True

    it "should test basic types exist" $ do
      let tripletId = TripletId undefined
      show tripletId `shouldContain` "TripletId"