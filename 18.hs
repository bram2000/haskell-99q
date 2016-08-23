import Test.Hspec
import Test.QuickCheck
import Data.List

main :: IO ()
main = hspec $ do
    describe "Bram.split" $ do
        it "slice from 3 to 7" $ do
            slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

slice :: [a] -> Int -> Int -> [a]
slice xs start end = take (end-start+1) (drop (start-1) xs)

