import Test.Hspec
import Test.QuickCheck
import Data.List

main :: IO ()
main = hspec $ do
    describe "Bram.split" $ do
        it "slice from 3 to 7" $ do
            slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

    describe "Bram.rotate" $ do
        it "rotate 3" $ do
            rotate "abcdefg" 3 `shouldBe` "defgabc"

    describe "Bram.rotate" $ do
        it "rotate -2" $ do
            rotate "abcdefg" (-2) `shouldBe` "fgabcde"

slice :: [a] -> Int -> Int -> [a]
slice xs start end = take (end-start+1) (drop (start-1) xs)

rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs i
    | i>0 = rotate (tail(xs) ++ [head xs]) (i-1)
    | i<0 = rotate (reverse((tail(reverse(xs))) ++ [head(reverse xs)])) (i+1)
