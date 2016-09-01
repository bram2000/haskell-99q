import Control.Exception.Base
import Test.Hspec
import Test.QuickCheck
import Data.List

main :: IO ()
main = hspec $ do
    describe "Bram.removeAt" $ do
        it "removeAt 2" $ do
            removeAt 2 "abcd" `shouldBe` ('b',"acd")

    describe "Bram.removeAt error" $ do
        it "removeAt -1" $ do
            evaluate(removeAt (-1) "abcd") `shouldThrow` anyErrorCall

    describe "Bram.removeAt error" $ do
        it "removeAt 10" $ do
            evaluate(removeAt 10 "abcd") `shouldThrow` anyErrorCall

removeAt :: Int -> [a] -> (a,[a])
removeAt n xs
    | (n>0 && n<=length(xs)) = (xs !! (n-1),(take (n-1) xs) ++ (drop n xs))
    | otherwise              = error "Index out of bounds!"
