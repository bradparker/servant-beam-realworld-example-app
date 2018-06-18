module ExampleSpec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "An example spec" $
    it "asserts things" $
      length ("A string" :: String) `shouldBe` 8
