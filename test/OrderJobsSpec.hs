{-# LANGUAGE ScopedTypeVariables #-}
module OrderJobsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import OrderJobs (sort, (.=>), independend)


spec :: Spec
spec = do
  describe "sort" $ do
    context "given a single job with no dependencies" $
      it "returns the single job" $ do
        sort ["a" .=> Nothing] `shouldBe` ["a"]
        
    it "some property holds" $ property $
      \(str :: String) -> str == str


----------------------------------------------------------------------

main :: IO ()
main = hspec spec


