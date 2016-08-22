import Test.Hspec
import Test.QuickCheck
import Data.List

main :: IO ()
main = hspec $ do
    describe "Bram.Dupli" $ do
        it "dupli test" $ do
            dupli "abc" `shouldBe` "aabbcc"

        it "dupli test numbers" $ do
            dupli [1, 2, 3] `shouldBe` [1, 1, 2, 2, 3, 3]

    describe "Bram.Repli" $ do
        it "repli test numbers" $ do
            repli "abc" 3 `shouldBe` "aaabbbccc"

dupli :: [a] -> [a]
dupli = foldr (\y acc -> y:y:acc) []

repli :: [a] -> Int -> [a]
repli x n = concatMap (replicate n) x
