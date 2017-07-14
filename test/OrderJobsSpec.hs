{-# LANGUAGE ScopedTypeVariables #-}
module OrderJobsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.List

import OrderJobs (sort, (.=>), independend)


spec :: Spec
spec = do
  describe "sort" $ do

    context "given no input" $ do
      let input = []
      it "returns an empty list" $ do
        let Right result = sort input
        result `shouldBe` []
    
    context "given a single job with no dependencies" $ do
      let input = [independend "a"]
      it "returns the single job" $ do
        let Right result = sort input
        result `shouldBe` ["a"]

    context "given three jobs without dependencies" $ do
      let input = map independend ["a","b","c"]
      it "returns the jobs in no significant order" $ do
        let Right result = sort input
        result `shouldSatisfy` (setEqual ["a","b","c"])

    context "given three jobs with a single dependency b=> c" $ do
      let input = [independend "a", "b" .=> "c", independend "c"]
      it "returns a list with all three jobs where b comes before c" $ do
        let Right result = sort input
        result `shouldSatisfy` (setEqual ["a","b","c"])
        result `shouldSatisfy` ("a" `earlierThan` "b")

    context "given multiple jobs with multiple dependencies" $ do
      let input =
            [ independend "a"
            , "b" .=> "c"
            , "c" .=> "f"
            , "d" .=> "a"
            , "e" .=> "b"
            , independend "f"
            ]
      it "should return a sequence that positions f before c, c before b, b before e and a before d containing all six jobs abcdef" $ do
        let Right result = sort input
        result `shouldSatisfy` (setEqual ["a","b","c","d","e","f"])
        result `shouldSatisfy` ("f" `earlierThan` "c")
        result `shouldSatisfy` ("c" `earlierThan` "b")
        result `shouldSatisfy` ("b" `earlierThan` "e")
        result `shouldSatisfy` ("a" `earlierThan` "d")

    context "given multiple jobs with a self-referential job" $ do
      let input = [independend "a", independend "b", "c" .=> "c" ]
      it "should return an error stating that jobs can’t depend on themselves" $
        sort input `shouldBe` Left "c depends on itself"

    context "given a circular dependency chain" $ do
      let input =
            [ independend "a"
            , "b" .=> "c"
            , "c" .=> "f"
            , "d" .=> "a"
            , independend "e"
            , "f" .=> "b"
            ]
      it "the result should be an error stating that jobs can’t have circular dependencies" $
        sort input `shouldBe` Left "cycle found"
        


----------------------------------------------------------------------

setEqual :: Ord a => [a] -> [a] -> Bool
setEqual xs ys = Data.List.sort xs == Data.List.sort ys


earlierThan :: Eq a => a -> a -> [a] -> Bool
earlierThan a b [] = True
earlierThan a b (x:xs)
  | x == b = False
  | x == a = True
  | otherwise = earlierThan a b xs


----------------------------------------------------------------------

main :: IO ()
main = hspec spec


