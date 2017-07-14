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
      it "returns the jobs in no significant order" $ do
        sort input `shouldSatisfy` (\res -> all (`elem` res) ["a","b","c"])

    context "given three jobs with a single dependency b=> c" $ do
      let input = [independend "a", "b" .=> "c", independend "c"]
      it "returns a list with all three jobs where b comes before c" $ do
        let result = sort input
        result `shouldSatisfy` (\res -> all (`elem` res) ["a","b","c"])
        result `shouldSatisfy` ((== 3) . length)
        result `shouldSatisfy` ("a" `earlierThan` "b")

        
    it "some property holds" $ property $
      \(str :: String) -> pending


----------------------------------------------------------------------

earlierThan :: Eq a => a -> a -> [a] -> Bool
earlierThan a b [] = True
earlierThan a b (x:xs)
  | x == b = False
  | x == a = True
  | otherwise = earlierThan a b xs


----------------------------------------------------------------------

main :: IO ()
main = hspec spec


