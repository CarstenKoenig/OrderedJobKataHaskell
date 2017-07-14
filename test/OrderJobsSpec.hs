{-# LANGUAGE ScopedTypeVariables #-}
module OrderJobsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import OrderJobs (sort, (.=>), independend)


spec :: Spec
spec = do
  describe "sort" $ do

    context "given no input" $ do
      let input = []
      it "returns an empty list" $
        sort input `shouldBe` []
    
    context "given a single job with no dependencies" $ do
      let input = [independend "a"]
      it "returns the single job" $
        sort input `shouldBe` ["a"]

    context "given three jobs without dependencies" $ do
      let input = map independend ["a","b","c"]
      it "returns the jobs in no significant order" $
        sort input `shouldSatisfy` (\res -> all (`elem` res) ["a","b","c"])

        
    it "some property holds" $ property $
      \(str :: String) -> pending


----------------------------------------------------------------------

main :: IO ()
main = hspec spec


