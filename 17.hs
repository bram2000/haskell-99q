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

    describe "Bram.DropEvery" $ do
        it "dropEvery 3rd elem" $ do
            dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

    describe "Bram.split" $ do
        it "split first 3" $ do
            split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

split :: [a] -> Int -> ([a],[a])
split xs     0 = ([],xs)
split (x:xs) n | n > 0 = ([x] ++ fst (split xs (n-1)),snd (split xs (n-1)))

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = if (length chunk) < n
    then chunk
    else (reverse . tail . reverse $ chunk) ++ (dropEvery rest n)
    where (chunk, rest) = splitAt n xs

dupli :: [a] -> [a]
dupli = foldr (\y acc -> y:y:acc) []

repli :: [a] -> Int -> [a]
repli x n = concatMap (replicate n) x
