module Test.Launcher.Environment
       ( spec
       ) where

import           Universum

import           Test.Hspec (Expectation, Spec, describe, it, shouldSatisfy)

import           Launcher.Environment (parse)

spec :: Spec
spec = describe "Test.Launcher.Environment" $ do
    it "handles sample-1" unitParserSample1

unitParserSample1 :: Expectation
unitParserSample1 = parse input `shouldSatisfy` isRight
  where
    input = ""
