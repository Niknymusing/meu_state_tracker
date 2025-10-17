module Main where

import Test.Hspec

import qualified MEU.Core.TripletSpec as TripletSpec
import qualified MEU.WS.StateTrackerSpec as StateTrackerSpec
import qualified MEU.Transforms.RefinementSpec as RefinementSpec

main :: IO ()
main = hspec $ do
  describe "MEU Framework WS State Tracker" $ do
    TripletSpec.spec
    StateTrackerSpec.spec
    RefinementSpec.spec