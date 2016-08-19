import Test.Hspec
import Test.QuickCheck
import Data.List

main :: IO ()
main = hspec $ do
    describe "Bram.Encode" $ do
        it "pack list" $ do
            pack "aaabbc" `shouldBe` ["aaa","bb","c"]

        it "encode list" $ do
            encode "aaaabbbbbcccaaaaagggg" `shouldBe` [(4,'a'),(5,'b'),(3,'c'),(5,'a'),(4,'g')]

        it "encode list (foldr)" $ do
            encodeFoldr "aaaabbbbbcccaaaaagggg" `shouldBe` [(4,'a'),(5,'b'),(3,'c'),(5,'a'),(4,'g')]

        it "encode-modifited list" $ do
            encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

pack :: Eq a => [a] -> [[a]]

pack []  = []
pack [x] = [[x]]
pack (x:y:xs) = if x == y
    then (x:(head (pack (y:xs)))):(tail (pack (y:xs)))
    else [x]:(pack (y:xs))


encode :: [Char] -> [(Int, Char)]

--encode = length . pack

--encode [a] ==> Char  = [(1,'a')(2,'b')]
--

encode [] = []
encode x  = (count,letter):(encode . dropWhile (== letter) $ x)
    where
        letter = (head . head . pack $ x)
        count  = (length . head . pack $ x)

encode' [] = []
encode' (x:xs) = (length $ x : takeWhile (==x) xs, x)
                 : encode' (dropWhile (==x) xs)


-- foldr solution...

encodeFoldr :: [Char] -> [(Int, Char)]
encodeFoldr x = fold . pack $ x where
    fold = foldr (\x y -> (length x, head x) : y) []

