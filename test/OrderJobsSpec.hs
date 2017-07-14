module OrderJobsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import OrderJobs (sort)


spec :: Spec
spec = do
  describe "sort" $ do
    context "given a single job with no dependencies" $
      it "returns the single job" $ do
        sort "a =>" `shouldBe` "a"
        
    it "is idempotent" $ property $
      \str -> sort str === sort (sort str)


----------------------------------------------------------------------

main :: IO ()
main = hspec spec


