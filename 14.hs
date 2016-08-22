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

dupli :: [a] -> [a]
dupli = foldr (\y acc -> y:y:acc) []
