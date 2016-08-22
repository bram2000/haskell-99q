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

        it "decode-modified test" $ do
            decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"

        it "direct-encode-modifited list" $ do
            encodeDirect "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

data Piece = Multiple Int Char | Single Char deriving (Show, Eq)

encodeModified :: [Char] -> [Piece]
encodeModified x = fold . encode $ x
    where
        fold = foldr (\(x,y) acc -> (case x of
                                        1 -> Single y
                                        _ -> Multiple x y) : acc) []

decodeModified :: [Piece] -> [Char]
decodeModified [] = []
decodeModified ((Multiple num val):xs) = (replicate num val) ++ (decodeModified xs)
decodeModified ((Single val):xs) = [val] ++ (decodeModified xs)

encodeDirect :: [Char] -> [Piece]
encodeDirect [] = []
encodeDirect (x:xs)
    | len==1    = (Single x):encodeDirect rest
    | otherwise = (Multiple len (head match)):encodeDirect rest
    where
            (match,rest) = span (==x) xs
            len = (length match) + 1

--instance Show Piece where
--    show (Single val) = show val
--    show (Multiple num val) = show (num,val)

pack :: Eq a => [a] -> [[a]]
pack []  = []
pack [x] = [[x]]
pack (x:y:xs) = if x == y
    then (x:(head (pack (y:xs)))):(tail (pack (y:xs)))
    else [x]:(pack (y:xs))


encode :: [Char] -> [(Int, Char)]
encode [] = []
encode x  = (count,letter):(encode . dropWhile (== letter) $ x)
    where
        letter = (head . head . pack $ x)
        count  = (length . head . pack $ x)

-- foldr solution...

encodeFoldr :: [Char] -> [(Int, Char)]
encodeFoldr x = fold . pack $ x where
    fold = foldr (\x y -> (length x, head x) : y) []

