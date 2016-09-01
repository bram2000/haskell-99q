import Control.Exception.Base
import Test.Hspec
import Test.QuickCheck
import Data.List

main :: IO ()
main = hspec $ do
    describe "Bram.insertAt" $ do
        it "insertAt 2" $ do
            insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

    describe "Bram.insertAt error" $ do
        it "insertAt -1" $ do
            evaluate(insertAt 'X' "abcd" (-1)) `shouldThrow` anyErrorCall

    describe "Bram.insertAt" $ do
        it "insertAt 5" $ do
            insertAt 'X' "abcd" 5 `shouldBe` "abcdX"



insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n
      | (n>0 && n<=length(xs))     = take (n-1) xs ++ [x] ++ drop (length (xs) -n-1) xs
      | (n>0 && n==(length(xs)+1)) = take (n-1) xs ++ [x]
      | otherwise                  = error "Index out of bounds!"

--removeAt :: Int -> [a] -> (a,[a])
--removeAt n xs
--    | (n>0 && n<=length(xs)) = (xs !! (n-1),(take (n-1) xs) ++ (drop n xs))
--    | otherwise              = error "Index out of bounds!"
